#include "main.h"
#include "movegen.h"
#include "search.h"
#include "evaluation.h"
#include "movesorter.h"

#include <cstdlib>
#include <csetjmp>
#include <iostream>
#include <sstream>

#if defined(_WIN32) || defined(_WIN64)
#include <windows.h>
#endif

template<class T>
void Swap(T& a, T& b)
{
	T tmp = a;
	a = b;
	b = tmp;
}

const int DrawScore = 0;

// TODO: Use PSQ tables in SEE?
static const int seePieceValues[8] = { 0, 1, 3, 3, 5, 10, 10000, 0 };

Move Killers[MaxPly][2];
int History[16][64];

// This version of SEE does not calculate the exact material imbalance, it just returns true = winning or equal, false = losing
bool FastSee(const Position& position, const Move move, const Color us)
{
	const Square from = GetFrom(move);
	const Square to = GetTo(move);

	const Color them = FlipColor(us);

	ASSERT(position.Board[from] != PIECE_NONE);
	ASSERT(GetPieceColor(position.Board[from]) == us);
	ASSERT(position.Board[to] == PIECE_NONE || GetPieceColor(position.Board[from]) != GetPieceColor(position.Board[to]));

	const int fromValue = seePieceValues[GetPieceType(position.Board[from])];
	const int toValue = seePieceValues[GetPieceType(position.Board[to])];

	if (fromValue <= toValue)
	{
		return true;
	}

	if (GetMoveType(move) == MoveTypeEnPassent)
	{
		// e.p. captures are always pxp which is winning or equal.
		return true;
	}

	// We should only be making initially losing captures here
	ASSERT(fromValue > seePieceValues[PAWN] || GetMoveType(move) == MoveTypePromotion || toValue == 0);

	const Bitboard enemyPieces = position.Colors[them];

	// Pawn attacks
	// If any opponent pawns can capture back, this capture is not worthwhile.
	// This is because any pawn captures will return true above, so we must be using a knight or above.
	if (GetPawnAttacks(to, us) & position.Pieces[PAWN] & enemyPieces)
	{
		return false;
	}

	// Knight attacks
	Bitboard attackingPieces = GetKnightAttacks(to) & position.Pieces[KNIGHT];
	const int captureDeficit = fromValue - toValue;
	// If any opponent knights can capture back, and the deficit we have to make up is greater than the knights value,
	// it's not worth it.  We can capture on this square again, and the opponent doesn't have to capture back.
	if (captureDeficit > seePieceValues[KNIGHT] && (attackingPieces & enemyPieces))
	{
		return false;
	}

	// Bishop attacks
	Bitboard allPieces = position.Colors[us] | enemyPieces;
	XorClearBit(allPieces, from);

	attackingPieces |= GetBishopAttacks(to, allPieces) & (position.Pieces[BISHOP] | position.Pieces[QUEEN]);
	if (captureDeficit > seePieceValues[BISHOP] && (attackingPieces & position.Pieces[BISHOP] & enemyPieces))
	{
		return false;
	}

	// Pawn defenses
	// At this point, we are sure we are making a "losing" capture.  The opponent can not capture back with a
	// pawn.  They cannot capture back with a bishop/knight and stand pat either.  So, if we can capture with
	// a pawn, it's got to be a winning or equal capture.
	if (GetPawnAttacks(to, them) & position.Pieces[PAWN] & position.Colors[us])
	{
		return true;
	}

	// Rook attacks
	attackingPieces |= GetRookAttacks(to, allPieces) & (position.Pieces[ROOK] | position.Pieces[QUEEN]);
	if (captureDeficit > seePieceValues[ROOK] && (attackingPieces & position.Pieces[ROOK] & enemyPieces))
	{
		return false;
	}

	// King attacks
	attackingPieces |= GetKingAttacks(to) & position.Pieces[KING];

	// Make sure our original attacking piece is not included in the set of attacking pieces
	attackingPieces &= allPieces;

	if (!attackingPieces)
	{
		// No pieces attacking us, we have won
		return true;
	}

	// At this point, we have built a bitboard of all the pieces attacking the "to" square.  This includes
	// potential x-ray attacks from removing the "from" square piece, which we used to do the capture.

	// We are currently winning the amount of material of the captured piece, time to see if the opponent
	// can get it back somehow.  We assume the opponent can capture our current piece in this score, which
	// simplifies the later code considerably.
	int seeValue = toValue - fromValue;

	for (;;)
	{
		int capturingPieceValue = -1;

		// Find the least valuable piece of the opponent that can attack the square
		for (PieceType capturingPiece = KNIGHT; capturingPiece <= KING; capturingPiece++)
		{
			const Bitboard attacks = position.Pieces[capturingPiece] & attackingPieces & position.Colors[them];
			if (attacks)
			{
				// They can capture with a piece
				capturingPieceValue = seePieceValues[capturingPiece];

				const Square attackingSquare = GetFirstBitIndex(attacks);
				XorClearBit(attackingPieces, attackingSquare);
				XorClearBit(allPieces, attackingSquare);
				break;
			}
		}

		if (capturingPieceValue == -1)
		{
			// The opponent can't capture back - we win
			return true;
		}

		// Now, if seeValue < 0, the opponent is winning.  If even after we take their piece,
		// we can't bring it back to 0, then we have lost this battle.
		seeValue += capturingPieceValue;
		if (seeValue < 0)
		{
			return false;
		}

		// Add any x-ray attackers
		attackingPieces |=
			((GetBishopAttacks(to, allPieces) & (position.Pieces[BISHOP] | position.Pieces[QUEEN])) |
				(GetRookAttacks(to, allPieces) & (position.Pieces[ROOK] | position.Pieces[QUEEN]))) & allPieces;

		// Our turn to capture
		capturingPieceValue = -1;

		// Find the least valuable piece of the opponent that can attack the square
		for (PieceType capturingPiece = KNIGHT; capturingPiece <= KING; capturingPiece++)
		{
			const Bitboard attacks = position.Pieces[capturingPiece] & attackingPieces & position.Colors[us];
			if (attacks)
			{
				// We can capture with a piece
				capturingPieceValue = seePieceValues[capturingPiece];

				const Square attackingSquare = GetFirstBitIndex(attacks);
				XorClearBit(attackingPieces, attackingSquare);
				XorClearBit(allPieces, attackingSquare);
				break;
			}
		}

		if (capturingPieceValue == -1)
		{
			// We can't capture back, we lose :(
			return false;
		}

		// Assume our opponent can capture us back, and if we are still winning, we can stand-pat
		// here, and assume we've won.
		seeValue -= capturingPieceValue;
		if (seeValue >= 0)
		{
			return true;
		}

		// Add any x-ray attackers
		attackingPieces |=
			((GetBishopAttacks(to, allPieces) & (position.Pieces[BISHOP] | position.Pieces[QUEEN])) |
				(GetRookAttacks(to, allPieces) & (position.Pieces[ROOK] | position.Pieces[QUEEN]))) & allPieces;

	}
}

void StableSortMoves(Move* moves, int* moveScores, int moveCount)
{
	// Stable sort the moves
	for (int i = 1; i < moveCount; i++)
	{
		int value = moveScores[i];
		Move move = moves[i];

		int j = i - 1;
		for (; j >= 0 && moveScores[j] < value; j--)
		{
			moveScores[j + 1] = moveScores[j];
			moves[j + 1] = moves[j];
		}

		moveScores[j + 1] = value;
		moves[j + 1] = move;
	}
}

inline bool SafePruneFromHash(const HashEntry* hashEntry, const int ply, const int beta)
{
	if (hashEntry->Depth >= (ply / OnePly))
	{
		const int hashFlags = hashEntry->GetHashFlags();
		return (hashFlags == HashFlagsExact ||
			(hashEntry->Score >= beta && hashFlags == HashFlagsBeta) ||
			(hashEntry->Score < beta && hashFlags == HashFlagsAlpha));
	}
	return false;
}

bool IsPassedPawnPush(const Position& position, const Move move)
{
	const Square from = GetFrom(move);
	if (GetPieceType(position.Board[from]) != PAWN)
	{
		return false;
	}

	const Bitboard theirPawns = position.Pieces[PAWN] & position.Colors[FlipColor(position.color)];

	extern Bitboard PassedPawnBitboards[64][2];
	return (PassedPawnBitboards[GetTo(move)][position.color] & theirPawns) == 0;
}

const int qPruningWeight[8] = { 900, 80, 325, 325, 500, 975, 0, 0 };

int QSearchCheck(Position& position, int alpha, const int beta) {
	if (CheckUp())
		return DrawScore;
	if (position.IsDraw())
		return DrawScore;
	MoveSorter<64> moves(position);
	moves.GenerateCheckEscape();
	if (moves.GetMoveCount() == 0)
		return 1 - MATE;

	int bestScore = -INF;

	Move move;
	while ((move = moves.NextQMove()) != 0)
	{
		MoveUndo moveUndo;
		position.MakeMove(move, moveUndo);

		ASSERT(!position.CanCaptureKing());

		int value;
		if (position.IsInCheck())
		{
			value = -QSearchCheck(position, -beta, -alpha);
		}
		else
			value = -QSearch(position, -beta, -alpha);//rap

		position.UnmakeMove(move, moveUndo);

		if (value > bestScore)
		{
			bestScore = value;
			if (alpha < value)
			{
				alpha = value;
				if (alpha >= beta)
					return value;
			}
		}
	}

	return bestScore;
}

int QSearch(Position& position, int alpha, const int beta)
{
	ASSERT(!position.IsInCheck());

	if (CheckUp())
		return 0;

	if (position.IsDraw())
		return DrawScore;

	const bool isCutNode = alpha + 1 == beta;

	HashEntry* hashEntry;
	Move hashMove = 0;
	if (ProbeHash(position.Hash, hashEntry))
	{
		if (isCutNode && SafePruneFromHash(hashEntry, 0, beta))
			return hashEntry->Score;
		hashMove = hashEntry->Move;
	}

	EvalInfo evalInfo;
	int eval = Evaluate(position, evalInfo);

	if (alpha < eval)
	{
		alpha = eval;
		if (alpha >= beta)
		{
			StoreHash(position.Hash, eval, 0, 0, HashFlagsBeta);
			return eval;
		}
	}

	const int optimisticValue = eval + 90 + (evalInfo.KingDanger[FlipColor(position.color)] ? 100 : 0);

	MoveSorter<64> moves(position);
	moves.GenerateCaptures();

	Move move;
	while ((move = moves.NextQMove()) != 0)
	{
		const bool seePrune = !FastSee(position, move, position.color);

		const int pruneValue = optimisticValue + qPruningWeight[GetPieceType(position.Board[GetTo(move)])];
		const bool isPassedPawnPush = IsPassedPawnPush(position, move);

		MoveUndo moveUndo;
		position.MakeMove(move, moveUndo);

		if (!position.CanCaptureKing())
		{
			int value;

			// Search this move
			if (position.IsInCheck())
			{
				value = -QSearchCheck(position, -beta, -alpha);//rap
			}
			else
			{
				if (pruneValue < alpha &&
					isCutNode &&
					move != hashMove &&
					!isPassedPawnPush &&
					GetMoveType(move) != MoveTypePromotion)
				{
					value = pruneValue;
				}
				else
				{
					if (isCutNode && seePrune && move != hashMove)
						value = eval;
					else
						value = -QSearch(position, -beta, -alpha);
				}
			}

			position.UnmakeMove(move, moveUndo);

			if (value > eval)
			{
				eval = value;
				if (alpha < value)
				{
					alpha = value;
					if (alpha >= beta)
						return value;
				}
			}
		}
		else
			position.UnmakeMove(move, moveUndo);
	}
	return eval;
}

bool InputAvailable() {
	static bool pipe = false;
	static HANDLE hstdin = 0;
	unsigned long dw = 0;
	if (!hstdin) {
		hstdin = GetStdHandle(STD_INPUT_HANDLE);
		pipe = !GetConsoleMode(hstdin, &dw);
		if (!pipe)
		{
			SetConsoleMode(hstdin, dw & ~(ENABLE_MOUSE_INPUT | ENABLE_WINDOW_INPUT));
			FlushConsoleInputBuffer(hstdin);
		}
		else
		{
			setvbuf(stdin, NULL, _IONBF, 0);
			setvbuf(stdout, NULL, _IONBF, 0);
		}
	}
	if (pipe)
		PeekNamedPipe(hstdin, 0, 0, 0, &dw, 0);
	else
		GetNumberOfConsoleInputEvents(hstdin, &dw);
	return dw > 1;
}

bool CheckUp() {
	if (!(++info.nodes & 0xffff)) {
		if (info.timeLimit && GetTimeMs() - info.timeStart >= info.timeLimit)
			info.stop = true;
		if (info.nodesLimit && info.nodes >= info.nodesLimit)
			info.stop = true;
		if (InputAvailable()) {
			std::string line;
			getline(std::cin, line);
			if (line == "stop")
				info.stop = true;
		}
	}
	return info.stop;
}

int Search(Position& position, const int beta, const int depth, const int ply, const int flags, const bool inCheck)
{
	ASSERT(depth > 0);
	ASSERT(inCheck ? position.IsInCheck() : !position.IsInCheck());

	if (CheckUp())
		return 0;

	if (position.IsDraw())
		return DrawScore;

	EvalInfo evalInfo;
	int evaluation = Evaluate(position, evalInfo);
	if (ply >= MAX_PLY)
		return evaluation;

	HashEntry* hashEntry;
	Move hashMove = 0;
	if (ProbeHash(position.Hash, hashEntry))
	{
		if (SafePruneFromHash(hashEntry, depth, beta))
			return hashEntry->Score;

		hashMove = hashEntry->Move;
	}

	if (!inCheck)
	{
		evaluation = Evaluate(position, evalInfo);

		// Try razoring
		if (depth <= OnePly * 4 &&
			hashMove == 0 &&
			evaluation < beta - (200 + 2 * depth))
		{
			int value = QSearch(position, beta - 1, beta);
			if (value < beta)
				return value;
		}

		if (depth > OnePly &&
			!(flags & 1) &&
			evaluation >= beta &&
			// Make sure we don't null move if we don't have any heavy pieces left
			evalInfo.GamePhase[position.color] > 0)
		{
			// Attempt to null-move
			MoveUndo moveUndo;
			position.MakeNullMove(moveUndo);

			const int R = 3 + (depth >= 5 * OnePly ? (depth / OnePly) / 4 : 0);
			const int newDepth = depth - (R * OnePly);
			int score;
			if (newDepth <= 0)
			{
				score = -QSearch(position, -beta, 1 - beta);
			}
			else
			{
				score = -Search(position, 1 - beta, newDepth, ply + 1, 1, false);
			}

			position.UnmakeNullMove(moveUndo);

			if (score >= beta)
			{
				StoreHash(position.Hash, score, 0, newDepth, HashFlagsBeta);
				return score;
			}
		}
	}

	MoveSorter<256> moves(position);
	bool singular = false;
	if (!inCheck)
	{
		moves.InitializeNormalMoves(hashMove, Killers[ply][0], Killers[ply][1], depth >= OnePly * 2);
	}
	else
	{
		moves.GenerateCheckEscape();
		if (moves.GetMoveCount() == 0)
		{
			return ply - MATE;
		}
		else if (moves.GetMoveCount() == 1)
		{
			singular = true;
		}
	}

	const int futilityPruningDepth = OnePly * 3;

	int moveCount = 0;
	int bestScore = -INF;
	Move move;
	while ((move = moves.NextNormalMove()) != 0){
		const bool isPassedPawnPush = IsPassedPawnPush(position, move);

		MoveUndo moveUndo;
		position.MakeMove(move, moveUndo);

		if (!position.CanCaptureKing())
		{
			int value;

			// Search move
			const bool isChecking = position.IsInCheck();
			int newDepth;
			if (isChecking)
				newDepth = depth - (OnePly / 2);
			else if (singular)
				newDepth = depth;
			else
			{
				// Try futility pruning
				if (!inCheck &&
					!isPassedPawnPush &&
					moves.GetMoveGenerationState() == MoveGenerationState_QuietMoves &&
					depth <= futilityPruningDepth)
				{
					ASSERT(evaluation != MATE);

					if (depth < 2 * OnePly)
						value = evaluation + 250;
					else if (depth < 3 * OnePly)
						value = evaluation + 325;
					else
						value = evaluation + 475;

					if (value < beta)
					{
						position.UnmakeMove(move, moveUndo);

						if (value > bestScore)
						{
							bestScore = value;
							hashMove = move;
						}
						continue;
					}
				}

				// Apply late move reductions if the conditions are met.
				if (!inCheck &&
					!isPassedPawnPush &&
					moveCount >= 3 &&
					depth > 3 * OnePly &&
					moves.GetMoveGenerationState() == MoveGenerationState_QuietMoves)
				{
					int reduction = min(max(moveCount - 8, 0), 3 * OnePly);
					newDepth = depth - OnePly - reduction;
				}
				else
					newDepth = depth - OnePly;
			}

			if (newDepth <= 0)
			{
				if (isChecking)
					value = -QSearchCheck(position, -beta, 1 - beta);
				else
					value = -QSearch(position, -beta, 1 - beta);
			}
			else
				value = -Search(position, 1 - beta, newDepth, ply + 1, 0, isChecking);//rap2

			if (newDepth < depth - OnePly && value >= beta)
			{
				// Re-search if the reduced move actually has the potential to be a good move.
				ASSERT(!isChecking);
				ASSERT(!inCheck);

				newDepth = depth - OnePly;
				ASSERT(newDepth > 0);

				value = -Search(position, 1 - beta, newDepth, ply + 1, 0, isChecking);
			}

			position.UnmakeMove(move, moveUndo);

			moveCount++;

			if (bestScore < value)
			{
				bestScore = value;
				hashMove = move;

				if (value >= beta)
				{
					StoreHash(position.Hash, value, move, depth, HashFlagsBeta);

					// Update killers and history (only for non-captures)
					const Square to = GetTo(move);
					if (position.Board[to] == PIECE_NONE)
					{
						if (move != Killers[ply][0] &&
							GetMoveType(move) != MoveTypePromotion &&
							GetMoveType(move) != MoveTypeEnPassent)
						{
							Killers[ply][1] = Killers[ply][0];
							Killers[ply][0] = move;
						}

						// Update history board, which is [pieceType][to], to allow for better move ordering
						const int normalizedPly = depth / OnePly;
						const Square from = GetFrom(move);
						History[position.Board[from]][to] += normalizedPly * normalizedPly;
						if (History[position.Board[from]][to] >= INF)
						{
							History[position.Board[from]][to] /= 2;
						}
					}

					return value;
				}
			}
		}
		else
			position.UnmakeMove(move, moveUndo);
	}

	if (bestScore == -INF)
	{
		ASSERT(!inCheck);
		return DrawScore;
	}
	StoreHash(position.Hash, bestScore, hashMove, depth, HashFlagsAlpha);
	return bestScore;
}

int SearchPV(Position& position, int alpha, const int beta, const int depth, const int ply, const bool inCheck){

	if (depth <= 0)
		return QSearch(position, alpha, beta);

	if (CheckUp())
		return DrawScore;

	if (position.IsDraw())
		return DrawScore;

	HashEntry* hashEntry;
	Move hashMove = 0;
	if (ProbeHash(position.Hash, hashEntry))
		hashMove = hashEntry->Move;

	MoveSorter<256> moves(position);
	bool singular = false;

	if (!inCheck)
		moves.InitializeNormalMoves(hashMove, Killers[ply][0], Killers[ply][1], true);
	else
	{
		moves.GenerateCheckEscape();
		if (moves.GetMoveCount() == 0)
			return ply - MATE;
		else if (moves.GetMoveCount() == 1)
			singular = true;
	}

	const int originalAlpha = alpha;
	int bestScore = -INF;
	int moveCount = 0;

	Move move;
	while ((move = moves.NextNormalMove()) != 0)
	{
		const bool isPassedPawnPush = IsPassedPawnPush(position, move);

		MoveUndo moveUndo;
		position.MakeMove(move, moveUndo);

		if (!position.CanCaptureKing())
		{
			int value;

			// Search move
			const bool isChecking = position.IsInCheck();
			int newDepth;

			if (isChecking || singular)
				newDepth = depth;
			else
			{
				if (!inCheck &&
					moveCount >= 14 &&
					depth >= 3 * OnePly &&
					moves.GetMoveGenerationState() == MoveGenerationState_QuietMoves &&
					!isPassedPawnPush)
				{
					newDepth = depth - (OnePly * 2);
				}
				else
				{
					newDepth = depth - OnePly;
				}
			}

			if (bestScore == -INF)
			{
				value = -SearchPV(position, -beta, -alpha, newDepth, ply + 1, isChecking);
			}
			else
			{
				if (newDepth <= 0)
				{
					value = -QSearch(position, -alpha - 1, -alpha);
				}
				else
				{
					value = -Search(position, -alpha, newDepth, ply + 1, 0, isChecking);
				}

				if (value > alpha)
				{
					value = -SearchPV(position, -beta, -alpha, newDepth, ply + 1, isChecking);
				}
			}

			if (newDepth < depth - OnePly && value > alpha)
			{
				// Re-search if the reduced move actually has the potential to be a good move.
				ASSERT(!isChecking);
				ASSERT(!inCheck);

				newDepth = depth - OnePly;
				ASSERT(newDepth > 0);

				value = -SearchPV(position, -beta, -alpha, newDepth, ply + 1, isChecking);
			}

			position.UnmakeMove(move, moveUndo);
			moveCount++;

			if (value > bestScore)
			{
				bestScore = value;
				hashMove = move;

				if (value > alpha)
				{
					alpha = value;

					if (value >= beta)
					{
						StoreHash(position.Hash, value, move, depth, HashFlagsBeta);

						// Update killers (only for non-captures/promotions)
						const Square to = GetTo(move);
						if (position.Board[to] == PIECE_NONE)
						{
							if (move != Killers[ply][0] &&
								GetMoveType(move) != MoveTypePromotion &&
								GetMoveType(move) != MoveTypeEnPassent)
							{
								Killers[ply][1] = Killers[ply][0];
								Killers[ply][0] = move;
							}

							const Square from = GetFrom(move);
							if (History[position.Board[from]][to] >= INF)
							{
								History[position.Board[from]][to] /= 2;
							}
						}

						return value;
					}
				}
			}
		}
		else
		{
			position.UnmakeMove(move, moveUndo);
		}
	}

	if (bestScore == -INF)
	{
		ASSERT(!inCheck);
		// Stalemate
		return DrawScore;
	}

	StoreHash(position.Hash, bestScore, hashMove, depth, bestScore > originalAlpha ? HashFlagsExact : HashFlagsAlpha);

	return bestScore;
}

void PrintPV(Position& position, const Move move) {
	if (position.IsDraw())
		return;
	MoveUndo moveUndo;
	position.MakeMove(move, moveUndo);
	if (!position.CanCaptureKing()) {
		printf(" %s", MoveToUci(move).c_str());
		HashEntry* hashEntry;
		if (ProbeHash(position.Hash, hashEntry) && IsMovePseudoLegal(position, hashEntry->Move))
			PrintPV(position, hashEntry->Move);
	}
	position.UnmakeMove(move, moveUndo);
}

int SearchRoot(Position& position, Move* moves, int* moveScores, int moveCount, int alpha, const int beta, const int depth) {
	memset(History, 0, sizeof(History));
	int originalAlpha = alpha;
	int bestScore = -INF;

	for (int i = 0; i < moveCount; i++) {
		MoveUndo moveUndo;
		position.MakeMove(moves[i], moveUndo);

		const bool isChecking = position.IsInCheck();
		const int newDepth = isChecking ? depth : depth - OnePly;
		int value;
		if (bestScore == -INF)
			value = -SearchPV(position, -beta, -alpha, newDepth, 1, isChecking);
		else
		{
			if (newDepth <= 0)
				value = -QSearch(position, -beta, -alpha);
			else
				value = -Search(position, -alpha, newDepth, 1, 0, isChecking);

			// Research if value > alpha, as this means this node is a new PV node.
			if (value > alpha)
				value = -SearchPV(position, -beta, -alpha, newDepth, 1, isChecking);
		}

		position.UnmakeMove(moves[i], moveUndo);
		if (info.stop)
			break;

		// Update move scores
		if (value <= alpha)
			moveScores[i] = originalAlpha;
		else if (value >= beta)
			moveScores[i] = beta;
		else
			moveScores[i] = value;

		if (value > bestScore) {
			bestScore = value;
			if (info.post) {
				std::cout << "info depth " << depth << " score ";
				//std::cout << "cp " << value;
				if (abs(value) < MATE - MAX_PLY)
					std::cout << "cp " << value;
				else
					std::cout << "mate " << (value > 0 ? (MATE - value + 1) >> 1 : -(MATE + value) >> 1);
				const auto elapsed = GetTimeMs() - info.timeStart;
				std::cout << " time " << elapsed;
				std::cout << " nodes " << info.nodes;
				std::cout << " hashfull " << Permill() << " pv";
				PrintPV(position, moves[i]);
				std::cout << std::endl;
			}
			if (alpha < value) {
				alpha = value;
				if (alpha >= beta)
					return value;
			}
		}
	}

	return bestScore;
}

void SearchIterate(Position& rootPosition) {
	Position position;
	rootPosition.Clone(position);

	Move moves[256];
	int moveCount = GenerateLegalMoves(position, moves);

	// Initial sorting of root moves is done by q-search scores
	int moveScores[256];
	for (int i = 0; i < moveCount; i++)
	{
		MoveUndo moveUndo;
		position.MakeMove(moves[i], moveUndo);
		if (position.IsInCheck())
			moveScores[i] = -QSearchCheck(position, -MATE, MATE);
		else
			moveScores[i] = -QSearch(position, -MATE, MATE);
		position.UnmakeMove(moves[i], moveUndo);
	}

	StableSortMoves(moves, moveScores, moveCount);

	int alpha = -MATE, beta = MATE;


	// Iterative deepening loop
	for (int depth = 1; depth <= info.depthLimit; depth++) {
		for (int i = 0; i < moveCount; i++)
			moveScores[i] = -MATE;
		SearchRoot(position, moves, moveScores, moveCount, alpha, beta, depth * OnePly);
		StableSortMoves(moves, moveScores, moveCount);
		if (info.stop)
			break;
		if (info.timeLimit && GetTimeMs() - info.timeStart > info.timeLimit / 2)
			break;
	}
	if (info.post)
		printf("bestmove %s\n", MoveToUci(moves[0]).c_str());
}