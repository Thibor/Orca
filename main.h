#if _DEBUG
extern "C" {
	void __declspec(dllimport) __stdcall DebugBreak(void);
}
#define ASSERT(a) (!(a) ? DebugBreak() : 0)
#else
#define ASSERT(a)
#endif

#include <string>
#include <vector>
#include <algorithm>
#include <sstream>
#include <cstdlib>
#include <iostream>
#include <random>
#include <csetjmp>

typedef signed char s8;
typedef unsigned char u8;

typedef signed short s16;
typedef unsigned short u16;

typedef signed int s32;
typedef unsigned int u32;

typedef signed long long S64;
typedef unsigned long long U64;

// White is at the "bottom" of the bitboard, in the bits 56-63 for Rank 1 for example.
// So, white pawns move by subtracting 1 from their row/rank
typedef U64 Bitboard;

// Move 0-5: from, 6-11: to, 12-13: promotion type, 14-15: flags
typedef u16 Move;
typedef int Square;

typedef int Piece;
typedef int PieceType;
typedef int Color;

#define INF 32001
#define MATE 32000
#define MAX_PLY 64
#define NAME "Orca"
#define VERSION "2026-02-02"
#define START_FEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

const Move PromotionTypeKnight = 0 << 12;
const Move PromotionTypeBishop = 1 << 12;
const Move PromotionTypeRook = 2 << 12;
const Move PromotionTypeQueen = 3 << 12;
const Move PromotionTypeMask = 3 << 12;

const Move MoveTypeNone = 0 << 14;
const Move MoveTypePromotion = 1 << 14;
const Move MoveTypeCastle = 2 << 14;
const Move MoveTypeEnPassent = 3 << 14;
const Move MoveTypeMask = 3 << 14;

const int CastleFlagWhiteKing = 1;
const int CastleFlagWhiteQueen = 2;
const int CastleFlagBlackKing = 4;
const int CastleFlagBlackQueen = 8;
const int CastleFlagMask = 15;

enum Piece_Type
{
	PIECE_NONE,
	PAWN,
	KNIGHT,
	BISHOP,
	ROOK,
	QUEEN,
	KING
};

enum Color_Type
{
	WHITE,
	BLACK
};

// Files are columns
enum File_Type
{
	FILE_A,	FILE_B,	FILE_C,	FILE_D,	FILE_E,	FILE_F,	FILE_G,	FILE_H
};

// Ranks are rows.  Note that RANK_8 (Black home row) is index 0
enum Rank_Type
{
	RANK_8, RANK_7, RANK_6, RANK_5, RANK_4, RANK_3, RANK_2, RANK_1
};

struct SearchInfo {
	bool post = true;
	bool stop = false;
	int depthLimit = MAX_PLY;
	U64 timeStart = 0;
	U64 timeLimit = 0;
	U64 nodes = 0;
	U64 nodesLimit = 0;
};

extern  SearchInfo info;

const int OnePly = 1;
const int MaxPly = 99;
const int MaxThreads = 8;
bool CheckUp();
int Permill();

inline bool IsSquareValid(const Square square)
{
	return square >= 0 && square < 64;
}

inline Square MakeSquare(const int row, const int column)
{
	ASSERT(row >= 0 && row < 8);
	ASSERT(column >= 0 && column < 8);

	return (row << 3) | column;
}

inline Square FlipSquare(const Square square)
{
	ASSERT(IsSquareValid(square));
	return square ^ 070;
}

inline int GetRow(const Square square)
{
	ASSERT(IsSquareValid(square));
	return square >> 3;
}

inline int GetColumn(const Square square)
{
	ASSERT(IsSquareValid(square));
	return square & 7;
}

inline Color FlipColor(const Color color)
{
	ASSERT(color == WHITE || color == BLACK);
	return color ^ 1;
}

inline Piece MakePiece(const Color color, const PieceType piece)
{
	return (color << 3) | piece;
}

inline PieceType GetPieceType(const Piece piece)
{
	return piece & 7;
}

inline Color GetPieceColor(const Piece piece)
{
	return piece >> 3;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Bitboard operations
////////////////////////////////////////////////////////////////////////////////////////////////////

inline bool IsBitSet(const Bitboard board, const Square square)
{
	return (board >> square) & 1;
}

inline void SetBit(Bitboard &board, const Square square)
{
	board |= 1ULL << square;
}

inline void ClearBit(Bitboard &board, const Square square)
{
	board &= ~(1ULL << square);
}

inline void XorClearBit(Bitboard &board, const Square square)
{
	ASSERT(IsBitSet(board, square));
	board ^= 1ULL << square;
}

extern const int BitTable[64];

inline Square GetFirstBitIndex(const Bitboard b)
{
#ifndef X64
	return Square(BitTable[((b & -S64(b)) * 0x218a392cd3d5dbfULL) >> 58]); 
#else
	unsigned long index;
	_BitScanForward64(&index, b);
	return index;
#endif
}

inline Square PopFirstBit(Bitboard &b)
{
	const Bitboard bb = b;
	b &= (b - 1);
	return GetFirstBitIndex(bb);
}

inline int CountBitsSet(Bitboard b)
{
	b -= ((b >> 1) & 0x5555555555555555ULL);
	b = ((b >> 2) & 0x3333333333333333ULL) + (b & 0x3333333333333333ULL);
	b = ((b >> 4) + b) & 0x0F0F0F0F0F0F0F0FULL;
	b *= 0x0101010101010101ULL;
	return int(b >> 56);
}

inline int CountBitsSetMax15(Bitboard b)
{
	b -= (b >> 1) & 0x5555555555555555ULL;
	b = ((b >> 2) & 0x3333333333333333ULL) + (b & 0x3333333333333333ULL);
	b *= 0x1111111111111111ULL;
	return int(b >> 60);
}

inline int CountBitsSetFew(Bitboard b)
{
	int result = 0;
	while (b)
	{
		b &= (b - 1);
		result++;
	}
	return result;
}

inline Bitboard FlipBitboard(const Bitboard b)
{
	Bitboard result = 0;
	for (int row = 0; row < 8; row++)
	{
		result |= ((b >> (row * 8)) & 0xFF) << ((7 - row) * 8);
	}
	return result;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Move operations
////////////////////////////////////////////////////////////////////////////////////////////////////
inline Square GetFrom(const Move move)
{
	return move & 0x3F;
}

inline Square GetTo(const Move move)
{
	return (move >> 6) & 0x3F;
}

inline Move GetMoveType(const Move move)
{
	return move & MoveTypeMask;
}

inline PieceType GetPromotionMoveType(const Move move)
{
	const int promotionMove = move & PromotionTypeMask;
	if (promotionMove == PromotionTypeQueen)
	{
		return QUEEN;
	}
	else if (promotionMove == PromotionTypeKnight)
	{
		return KNIGHT;
	}
	else if (promotionMove == PromotionTypeRook)
	{
		return ROOK;
	}
	else if (promotionMove == PromotionTypeBishop)
	{
		return BISHOP;
	}
	// Whoops
	ASSERT(false);
	return PIECE_NONE;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Attack generation
////////////////////////////////////////////////////////////////////////////////////////////////////

extern Bitboard RowBitboard[8];
extern Bitboard ColumnBitboard[8];

extern Bitboard PawnMoves[2][64];
extern Bitboard PawnAttacks[2][64];
extern Bitboard KnightAttacks[64];

extern Bitboard BMask[64];
extern int BAttackIndex[64];
extern Bitboard BAttacks[0x1480];

extern const U64 BMult[64];
extern const int BShift[64];

extern Bitboard RMask[64];
extern int RAttackIndex[64];
extern Bitboard RAttacks[0x19000];

extern const U64 RMult[64];
extern const int RShift[64];

extern Bitboard KingAttacks[64];

inline Bitboard GetPawnMoves(const Square square, const Color color)
{
	return PawnMoves[color][square];
}

inline Bitboard GetPawnAttacks(const Square square, const Color color)
{
	return PawnAttacks[color][square];
}

inline Bitboard GetKnightAttacks(const Square square)
{
	return KnightAttacks[square];
}

inline Bitboard GetBishopAttacks(const Square square, const Bitboard blockers)
{
	const Bitboard b = blockers & BMask[square];
	return BAttacks[BAttackIndex[square] + ((b * BMult[square]) >> BShift[square])];
}

inline Bitboard GetRookAttacks(const Square square, const Bitboard blockers)
{
	const Bitboard b = blockers & RMask[square];
	return RAttacks[RAttackIndex[square] + ((b * RMult[square]) >> RShift[square])];
}

inline Bitboard GetQueenAttacks(const Square square, const Bitboard blockers)
{
	return GetBishopAttacks(square, blockers) | GetRookAttacks(square, blockers);
}

inline Bitboard GetKingAttacks(const Square square)
{
	return KingAttacks[square];
}

// Misc. attack functions
extern Bitboard SquaresBetween[64][64];

inline Bitboard GetSquaresBetween(const Square from, const Square to)
{
	return SquaresBetween[from][to];
}

struct MoveUndo
{
	U64 Hash;
	U64 PawnHash;
	PieceType Captured;
	int CastleFlags;
	Square EnPassent;
	int Fifty;
};

const int HashFlagsAlpha = 0;
const int HashFlagsBeta = 1;
const int HashFlagsExact = 2;
const int HashFlagsMask = 3;

struct HashEntry
{
	u32 Lock;
	s16 Score;
	Move Move;
	u8 Depth;
	u8 Extra;

	int GetHashFlags() const
	{
		return Extra & HashFlagsMask;
	}

	int GetHashDate() const
	{
		return Extra >> 4;
	}
};
#pragma pack(pop)

extern HashEntry* tt;
extern U64 ttMask;
extern int ttDate;

// Hash size in bytes
void InitializeHash(int hashSize);
void IncrementHashDate();

inline bool ProbeHash(const U64 hash, HashEntry*& result) {
	const U64 base = hash & ttMask;
	ASSERT(base <= ttMask);

	const u32 lock = hash >> 32;
	for (U64 i = base; i < base + 4; i++)
	{
		if (tt[i].Lock == lock)
		{
			result = &(tt[i]);
			return true;
		}
	}
	return false;
}

inline void StoreHash(const U64 hash, const s16 score, const Move move, int depth, const int flags)
{
	const U64 base = hash & ttMask;
	const u32 lock = hash >> 32;
	int bestScore = 512;
	U64 best=base;

	depth /= OnePly;

	for (U64 i = base; i < base + 4; i++)
	{
		if (tt[i].Lock == lock)
		{
			if (depth >= tt[i].Depth)
			{
				best = i;
				break;
			}
			if (tt[i].Move == 0)
			{
				tt[i].Move = move;
			}
			return;
		}

		int matchScore;
		if (tt[i].GetHashDate() != ttDate)
		{
			// We want to always allow overwriting of hash entries not from our hash date
			matchScore = tt[i].Depth;
		}
		else
		{
			// Otherwise, choose the hash entry with the lowest depth for overwriting
			matchScore = 256 + tt[i].Depth;
		}

		if (matchScore < bestScore)
		{
			bestScore = matchScore;
			best = i;
		}
	}

	tt[best].Lock = lock;
	tt[best].Move = move;
	tt[best].Score = score;
	tt[best].Depth = depth;
	tt[best].Extra = flags | (ttDate << 4);
}

class Position
{
public:
	U64 Hash;
	U64 PawnHash;

	Bitboard Pieces[8];
	Bitboard Colors[2];
	Square KingPos[2];

	int PsqEvalOpening;
	int PsqEvalEndgame;

	int CastleFlags;
	int Fifty;
	Color color;
	Square EnPassent;
	Piece Board[64];

	inline Bitboard GetAllPieces() const { return Colors[WHITE] | Colors[BLACK]; }

	static void StaticInitialize();
	void SetFen(const std::string& fen);
	std::string GetFen() const;
	void Clone(Position& other) const;

	// Debug only!
	void Flip();

	void MakeMove(const Move move, MoveUndo& moveUndo);
	void UnmakeMove(const Move move, const MoveUndo& moveUndo);

	void MakeNullMove(MoveUndo& moveUndo);
	void UnmakeNullMove(MoveUndo& moveUndo);

	inline bool IsSquareAttacked(const Square square, const Color them) const
	{
		return IsSquareAttacked(square, them, GetAllPieces());
	}
	inline bool IsSquareAttacked(const Square square, const Color them, const Bitboard allPieces) const
	{
		const Bitboard enemyPieces = Colors[them] & allPieces;

		if ((GetPawnAttacks(square, FlipColor(them)) & Pieces[PAWN] & enemyPieces) ||
			(GetKnightAttacks(square) & Pieces[KNIGHT] & enemyPieces))
		{
			return true;
		}

		const Bitboard bishopQueen = Pieces[BISHOP] | Pieces[QUEEN];
		if (GetBishopAttacks(square, allPieces) & bishopQueen & enemyPieces)
		{
			return true;
		}

		const Bitboard rookQueen = Pieces[ROOK] | Pieces[QUEEN];
		if (GetRookAttacks(square, allPieces) & rookQueen & enemyPieces)
		{
			return true;
		}

		if (GetKingAttacks(square) & Pieces[KING] & enemyPieces)
		{
			return true;
		}

		return false;
	}

	inline bool IsInCheck() const { return IsSquareAttacked(KingPos[color], FlipColor(color)); }
	inline bool CanCaptureKing() const { return IsSquareAttacked(KingPos[FlipColor(color)], color); }

	Bitboard GetAttacksTo(const Square square) const;
	Bitboard GetPinnedPieces(const Square square, const Color us) const;

	inline bool IsDraw() const
	{
		if (Fifty >= 100)
			return true;

		// Check our previous positions.  If the hash key matches, it is a draw.
		const int end = MoveDepth - Fifty;
		for (int i = MoveDepth - 4; i >= end; i -= 2)
		{
			if (DrawKeys[i] == Hash)
			{
				return true;
			}
		}
		return false;
	}

	inline void ResetMoveDepth()
	{
		MoveDepth = 0;
	}

private:
	int MoveDepth;		// used for tracking repitition draws
	U64 DrawKeys[256];

	void VerifyBoard() const;
	U64 GetHash() const;
	U64 GetPawnHash() const;
	int GetPsqEval(int gameStage) const;

	static int RookCastleFlagMask[64];
	static U64 Zobrist[2][8][64];
	static U64 ZobristEP[64];
	static U64 ZobristCastle[16];
	static U64 ZobristToMove;
};

#define EVAL_FEATURE(featureName, value) const int (featureName) = (value);
#define EVAL_CONST const

extern int PsqTableOpening[16][64];
extern int PsqTableEndgame[16][64];

void InitializeEvaluation();

struct EvalInfo
{
	int GamePhase[2];
	bool KingDanger[2];
};

int Evaluate(const Position& position, EvalInfo& evalInfo);

template<class T>
inline const T& min(const T& a, const T& b) { return a < b ? a : b; }

template<class T>
inline const T& max(const T& a, const T& b) { return a > b ? a : b; }

bool FastSee(const Position& position, const Move move, const Color us);
int QSearch(Position& position, int alpha, const int beta);
int QSearchCheck(Position& position, int alpha, const int beta);
void SearchIterate(Position& position);
U64 GetTimeMs();
bool InputAvailable();
std::vector<std::string> SplitString(std::string s);
//std::vector<std::string> tokenize(const std::string& in, const std::string& tok);
Move UciToMove(Position& position, const std::string& moveString);
std::string SquareToUci(const Square square);
std::string MoveToUci(const Move move);

void InitializeBitboards();

inline int ScoreCaptureMove(const PieceType fromPiece, const PieceType toPiece)
{
	ASSERT(fromPiece != PIECE_NONE);
	ASSERT(toPiece != PIECE_NONE);

	return toPiece * 100 - fromPiece;
}
int ScoreCaptureMove(const Move move, const PieceType fromPiece, const PieceType toPiece);

int GenerateSliderMoves(const Position& position, Move* moves);
int GenerateQuietMoves(const Position& position, Move* moves);
int GenerateCaptureMoves(const Position& position, Move* moves, s16* moveScores);
int GenerateCheckingMoves(const Position& position, Move* moves);
int GenerateCheckEscapeMoves(const Position& position, Move* moves);
bool IsMovePseudoLegal(const Position& position, const Move move);

// Slow, and should not be used.
int GenerateLegalMoves(Position& position, Move* legalMoves);

// Move generation
inline Move GenerateMove(const Square from, const Square to)
{
	ASSERT(IsSquareValid(from));
	ASSERT(IsSquareValid(to));

	return from | (to << 6);
}

inline Move GeneratePromotionMove(const Square from, const Square to, const int promotionType)
{
	return GenerateMove(from, to) | promotionType | MoveTypePromotion;
}

inline Move GenerateCastleMove(const Square from, const Square to)
{
	return GenerateMove(from, to) | MoveTypeCastle;
}

inline Move GenerateEnPassentMove(const Square from, const Square to)
{
	return GenerateMove(from, to) | MoveTypeEnPassent;
}

enum MoveGenerationState
{
	MoveGenerationState_Hash,
	MoveGenerationState_GenerateWinningEqualCaptures,
	MoveGenerationState_WinningEqualCaptures,
	MoveGenerationState_Killer1,
	MoveGenerationState_Killer2,
	MoveGenerationState_GenerateQuietMoves,
	MoveGenerationState_QuietMoves,
	MoveGenerationState_GenerateLosingCaptures,
	MoveGenerationState_LosingCaptures,
	MoveGenerationState_CheckEscapes,
};

template<int maxMoves>
class MoveSorter
{
public:
	MoveSorter(const Position& position) : position(position)
	{
	}

	inline int GetMoveCount() const
	{
		return moveCount;
	}

	inline MoveGenerationState GetMoveGenerationState() const
	{
		return state;
	}

	inline void GenerateCaptures()
	{
		at = 0;
		moveCount = GenerateCaptureMoves(position, moves, moveScores);
		moves[moveCount] = 0; // Sentinel move
	}

	inline void GenerateCheckEscape()
	{
		at = 0;
		moveCount = GenerateCheckEscapeMoves(position, moves);
		moves[moveCount] = 0;

		state = MoveGenerationState_CheckEscapes;

		for (int i = 0; i < moveCount; i++)
		{
			const PieceType toPiece = GetPieceType(position.Board[GetTo(moves[i])]);

			moveScores[i] = toPiece != PIECE_NONE ?
				// Sort captures first
				ScoreCaptureMove(moves[i], GetPieceType(position.Board[GetFrom(moves[i])]), toPiece) :
				// Score non-captures as equal
				0;
		}
	}

	inline void GenerateChecks()
	{
		at = 0;
		moveCount = GenerateCheckingMoves(position, moves);
		moves[moveCount] = 0;
	}

	inline void InitializeNormalMoves(const Move hashMove, const Move killer1, const Move killer2, const bool generatePawnMoves)
	{
		at = 0;
		state = hashMove == 0 ? MoveGenerationState_GenerateWinningEqualCaptures : MoveGenerationState_Hash;

		this->generatePawnMoves = generatePawnMoves;

		this->hashMove = hashMove;
		this->killer1 = killer1;
		this->killer2 = killer2;
	}

	inline Move PickBestMove()
	{
		// We use a trick here instead of checking for at == moveCount.
		// The move initialization code stores a sentinel move at the end of the movelist.
		ASSERT(at <= moveCount);

		s16 bestScore = moveScores[at], bestMove = moves[at];
		for (int i = ++at; i < moveCount; i++)
		{
			ASSERT(moveScores[i] >= -MATE && moveScores[i] <= MATE);

			const s16 score = moveScores[i];
			if (score > bestScore)
			{
				const Move move = moves[i];
				moves[i] = bestMove;
				moveScores[i] = bestScore;
				bestMove = move;
				bestScore = score;
			}
		}

		ASSERT(at >= moveCount || (moveScores[at - 1] >= moveScores[at]));

		return bestMove;
	}

	inline Move NextQMove()
	{
		return PickBestMove();
	}

	inline Move NextNormalMove()
	{
		ASSERT(state >= MoveGenerationState_Hash && state <= MoveGenerationState_CheckEscapes);

		switch (state)
		{
		case MoveGenerationState_Hash:
			if (IsMovePseudoLegal(position, hashMove))
			{
				state = MoveGenerationState_GenerateWinningEqualCaptures;
				return hashMove;
			}

			// Intentional fall-through

		case MoveGenerationState_GenerateWinningEqualCaptures:
			GenerateCaptures();
			losingCapturesCount = 0;

			state = MoveGenerationState_WinningEqualCaptures;

			// Intentional fall-through

		case MoveGenerationState_WinningEqualCaptures:
			while (at < moveCount)
			{
				const Move bestMove = PickBestMove();
				ASSERT(bestMove != 0);

				if (bestMove == hashMove)
				{
					continue;
				}

				// Losing captures go off to the back of the list, winning/equal get returned.  They will
				// have been scored correctly already.
				if (FastSee(position, bestMove, position.color))
				{
					return bestMove;
				}
				else
				{
					losingCaptures[losingCapturesCount++] = bestMove;
				}
			}

			// Intentional fall-through

		case MoveGenerationState_Killer1:
			if (killer1 != hashMove &&
				IsMovePseudoLegal(position, killer1) &&
				position.Board[GetTo(killer1)] == PIECE_NONE)
			{
				state = MoveGenerationState_Killer2;
				return killer1;
			}

			// Intentional fall-through

		case MoveGenerationState_Killer2:
			if (killer2 != hashMove &&
				IsMovePseudoLegal(position, killer2) &&
				position.Board[GetTo(killer2)] == PIECE_NONE)
			{
				state = MoveGenerationState_GenerateQuietMoves;
				return killer2;
			}

			// Intentional fall-through

		case MoveGenerationState_GenerateQuietMoves:
			if (generatePawnMoves)
			{
				moveCount = GenerateQuietMoves(position, moves);
			}
			else
			{
				moveCount = GenerateSliderMoves(position, moves);
			}
			at = 0;

			for (int i = 0; i < moveCount; i++)
			{
				int historyScore = History[position.Board[GetFrom(moves[i])]][GetTo(moves[i])];
				if (historyScore > MATE) historyScore = MATE;
				moveScores[i] = historyScore;
			}

			state = MoveGenerationState_QuietMoves;
			// Intentional fall-through

		case MoveGenerationState_QuietMoves:
			while (at < moveCount)
			{
				const Move bestMove = PickBestMove();
				if (bestMove == hashMove || bestMove == killer1 || bestMove == killer2)
				{
					continue;
				}
				return bestMove;
			}

			// Intentional fall-through

		case MoveGenerationState_GenerateLosingCaptures:
			at = 0;
			state = MoveGenerationState_LosingCaptures;

			// Intentional fall-through

		case MoveGenerationState_LosingCaptures:
			while (at < losingCapturesCount)
			{
				const Move bestMove = losingCaptures[at++];
				if (bestMove == hashMove)
				{
					continue;
				}
				return bestMove;
			}
			break;

		case MoveGenerationState_CheckEscapes:
			return PickBestMove();
		}

		return 0;
	}

private:

	Move moves[maxMoves];
	s16 moveScores[maxMoves];
	int moveCount;

	Move losingCaptures[32];
	int losingCapturesCount;

	bool generatePawnMoves;
	int at;
	const Position& position;
	MoveGenerationState state;
	Move hashMove, killer1, killer2;
};