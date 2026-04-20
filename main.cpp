#include "main.h"

using namespace std;

SearchInfo info;
HashEntry* tt = 0;
U64 ttMask = 0;
int ttDate = 0;
Position pos;

int PsqTableOpening[16][64];
int PsqTableEndgame[16][64];

// Material weights (in centipawns, which are directly used by the evaluation value)
EVAL_FEATURE(PawnOpening, 77);
EVAL_FEATURE(PawnEndgame, 101);
EVAL_FEATURE(KnightOpening, 325);
EVAL_FEATURE(KnightEndgame, 320);
EVAL_FEATURE(BishopOpening, 325);
EVAL_FEATURE(BishopEndgame, 320);
EVAL_FEATURE(RookOpening, 500);
EVAL_FEATURE(RookEndgame, 500);
EVAL_FEATURE(QueenOpening, 975);
EVAL_FEATURE(QueenEndgame, 990);

EVAL_FEATURE(BishopPairOpening, 50);
EVAL_FEATURE(BishopPairEndgame, 70);

// Psq weights (in millipawns, all are divided by 10 before being used in the psq-table)
EVAL_FEATURE(PawnColumnOpening, 54);
EVAL_FEATURE(PawnColumnEndgame, 0);
EVAL_FEATURE(KnightCenterOpening, 52);
EVAL_FEATURE(KnightCenterEndgame, 52);
EVAL_FEATURE(KnightRowOpening, 52);
EVAL_FEATURE(KnightRowEndgame, 0);
EVAL_FEATURE(KnightBackRowOpeningPenalty, 5);
EVAL_FEATURE(BishopCenterOpening, 19);
EVAL_FEATURE(BishopCenterEndgame, 31);
EVAL_FEATURE(BishopBackRowOpeningPenalty, 95);
EVAL_FEATURE(BishopDiagonalOpening, 41);
EVAL_FEATURE(RookColumnOpening, 30);
EVAL_FEATURE(RookColumnEndgame, 0);
EVAL_FEATURE(QueenCenterOpening, 3);
EVAL_FEATURE(QueenCenterEndgame, 40);
EVAL_FEATURE(QueenBackRowOpeningPenalty, 49);
EVAL_FEATURE(KingCenterOpening, 0);
EVAL_FEATURE(KingCenterEndgame, 120);
EVAL_FEATURE(KingColumnOpening, 99);
EVAL_FEATURE(KingColumnEndgame, 0);
EVAL_FEATURE(KingRowOpening, 97);
EVAL_FEATURE(KingRowEndgame, 0);

// Feature multipliers - weights from Toga
const int EmptyFeature[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
const int PawnColumn[8] = { -3, -1, +0, +1, +1, +0, -1, -3 };
const int KnightLine[8] = { -4, -2, +0, +1, +1, +0, -2, -4 };
const int KnightRow[8] = { +1, +2, +3, +2, +1, +0, -1, -2 };
const int BishopLine[8] = { -3, -1, +0, +1, +1, +0, -1, -3 };
const int RookColumn[8] = { -2, -1, +0, +1, +1, +0, -1, -2 };
const int QueenLine[8] = { -3, -1, +0, +1, +1, +0, -1, -3 };
const int KingLine[8] = { -3, -1, +0, +1, +1, +0, -1, -3 };
const int KingColumn[8] = { +3, +4, +2, +0, +0, +2, +4, +3 };
const int KingRow[8] = { -7, -6, -5, -4, -3, -2, +0, +1 };

void InitPiece(Piece piece,
	int weightOpening, int weightEndgame,
	const int center[8], int centerWeightOpening, int centerWeightEndgame,
	const int row[8], int rowWeightOpening, int rowWeightEndgame,
	const int column[8], int columnWeightOpening, int columnWeightEndgame)
{
	ASSERT(piece < 16);

	for (Square square = 0; square < 64; square++)
	{
		PsqTableOpening[piece][square] = weightOpening * 10; // Will be divided by 10 at the end

		PsqTableOpening[piece][square] +=
			(row[GetRow(square)] * rowWeightOpening) +
			(column[GetColumn(square)] * columnWeightOpening) +
			(center[GetRow(square)] * centerWeightOpening) +
			(center[GetColumn(square)] * centerWeightOpening);

		PsqTableEndgame[piece][square] = weightEndgame * 10; // Will be divided by 10 at the end

		PsqTableEndgame[piece][square] +=
			(row[GetRow(square)] * rowWeightEndgame) +
			(column[GetColumn(square)] * columnWeightEndgame) +
			(center[GetRow(square)] * centerWeightEndgame) +
			(center[GetColumn(square)] * centerWeightEndgame);
	}
}

void InitializePsqTable()
{
	for (Piece piece = 0; piece < 14; piece++)
	{
		for (Square square = 0; square < 64; square++)
		{
			PsqTableOpening[piece][square] = 0;
			PsqTableEndgame[piece][square] = 0;
		}
	}

	// Piece-square tables are initialized for white first, then flipped for black
	InitPiece(MakePiece(WHITE, PAWN),
		PawnOpening, PawnEndgame,
		EmptyFeature, 0, 0,
		EmptyFeature, 0, 0,
		PawnColumn, PawnColumnOpening, PawnColumnEndgame);

	InitPiece(MakePiece(WHITE, KNIGHT),
		KnightOpening, KnightEndgame,
		KnightLine, KnightCenterOpening, KnightCenterEndgame,
		KnightRow, KnightRowOpening, KnightRowEndgame,
		EmptyFeature, 0, 0);

	// Knight bank rank penalty
	for (Square square = MakeSquare(RANK_1, FILE_A); square <= MakeSquare(RANK_1, FILE_H); square++)
	{
		PsqTableOpening[MakePiece(WHITE, KNIGHT)][square] -= KnightBackRowOpeningPenalty;
	}

	InitPiece(MakePiece(WHITE, BISHOP),
		BishopOpening, BishopEndgame,
		BishopLine, BishopCenterOpening, BishopCenterEndgame,
		EmptyFeature, 0, 0,
		EmptyFeature, 0, 0);

	// Bishop bank rank penalty
	for (Square square = MakeSquare(RANK_1, FILE_A); square <= MakeSquare(RANK_1, FILE_H); square++)
	{
		PsqTableOpening[MakePiece(WHITE, BISHOP)][square] -= BishopBackRowOpeningPenalty;
	}

	InitPiece(MakePiece(WHITE, ROOK),
		RookOpening, RookEndgame,
		EmptyFeature, 0, 0,
		EmptyFeature, 0, 0,
		RookColumn, RookColumnOpening, RookColumnEndgame);

	InitPiece(MakePiece(WHITE, QUEEN),
		QueenOpening, QueenEndgame,
		QueenLine, QueenCenterOpening, QueenCenterEndgame,
		EmptyFeature, 0, 0,
		EmptyFeature, 0, 0);

	// Queen bank rank penalty
	for (Square square = MakeSquare(RANK_1, FILE_A); square <= MakeSquare(RANK_1, FILE_H); square++)
	{
		PsqTableOpening[MakePiece(WHITE, QUEEN)][square] -= QueenBackRowOpeningPenalty;
	}

	InitPiece(MakePiece(WHITE, KING),
		0, 0,	// Kings are not worth anything materially
		KingLine, KingCenterOpening, KingCenterEndgame,
		KingRow, KingRowOpening, KingRowEndgame,
		KingColumn, KingColumnOpening, KingColumnEndgame);

	// Normalize, and copy the tables to Black
	for (PieceType pieceType = PAWN; pieceType <= KING; pieceType++)
	{
		for (Square square = 0; square < 64; square++)
		{
			PsqTableOpening[MakePiece(WHITE, pieceType)][square] /= 10;
			PsqTableEndgame[MakePiece(WHITE, pieceType)][square] /= 10;
		}

		for (Square square = 0; square < 64; square++)
		{
			PsqTableOpening[MakePiece(BLACK, pieceType)][FlipSquare(square)] =
				-PsqTableOpening[MakePiece(WHITE, pieceType)][square];

			PsqTableEndgame[MakePiece(BLACK, pieceType)][FlipSquare(square)] =
				-PsqTableEndgame[MakePiece(WHITE, pieceType)][square];
		}
	}
}

U64 GetTimeMs() {
	return (clock() * 1000) / CLOCKS_PER_SEC;
}

void TTClear() {
	if (tt)
		memset(tt, 0, (size_t)((ttMask | 3) * sizeof(HashEntry)));
}

void InitializeHash(int hashSize) {
	hashSize *= 1000000;
	for (ttMask = 1; ttMask * sizeof(HashEntry) < hashSize; ttMask *= 2);
	ttMask /= 2;
	ttMask--;
	if (tt)
		free(tt);
	size_t allocSize = (size_t)(ttMask * sizeof(HashEntry));
	tt = (HashEntry*)malloc(allocSize);
	ttMask &= ~3;
	TTClear();
}

int Permill() {
	int pm = 0;
	for (int n = 0; n < 1000; n++) {
		if (tt[n].Lock)
			pm++;
	}
	return pm;
}

void IncrementHashDate() {
	ttDate = (ttDate + 1) & 0xf;
}

static void ResetInfo() {
	info.stop = false;
	info.post = true;
	info.nodes = 0;
	info.nodesLimit = 0;
	info.timeLimit = 0;
	info.depthLimit = MAX_PLY;
	info.timeStart = GetTimeMs();
}

vector<string> SplitString(string s) {
	vector<string> words;
	istringstream iss(s);
	string word;
	while (iss >> word)
		words.push_back(word);
	return words;
}

std::string SquareToUci(const Square square) {
	string result;
	result += GetColumn(square) + 'a';
	result += (RANK_1 - GetRow(square)) + '1';
	return result;
}

std::string MoveToUci(const Move move) {
	string result;
	result += SquareToUci(GetFrom(move));
	result += SquareToUci(GetTo(move));
	if (GetMoveType(move) == MoveTypePromotion) {
		switch (GetPromotionMoveType(move)) {
		case KNIGHT: result += "n"; break;
		case BISHOP: result += "b"; break;
		case ROOK: result += "r"; break;
		case QUEEN: result += "q"; break;
		}
	}

	return result;
}

Move UciToMove(Position& position, const std::string& moveString) {
	Move moves[256];
	int moveCount = GenerateLegalMoves(position, moves);
	for (int i = 0; i < moveCount; i++)
		if (MoveToUci(moves[i]) == moveString)
			return moves[i];
	ASSERT(false);
	return 0;
}

static void ParsePosition(string command) {
	string fen = START_FEN;
	stringstream ss(command);
	string token;
	ss >> token;
	if (token != "position")
		return;
	ss >> token;
	if (token == "startpos")
		ss >> token;
	else if (token == "fen") {
		fen = "";
		while (ss >> token && token != "moves")
			fen += token + " ";
		fen.pop_back();
	}
	pos.SetFen(fen);
	while (ss >> token) {
		MoveUndo moveUndo;
		pos.MakeMove(UciToMove(pos, token), moveUndo);
		if (pos.Fifty == 0)
			pos.ResetMoveDepth();
	}
}

static void ParseGo(string command) {
	stringstream ss(command);
	string token;
	ss >> token;
	if (token != "go")
		return;
	ResetInfo();
	int wtime = 0;
	int btime = 0;
	int winc = 0;
	int binc = 0;
	int movestogo = 32;
	while (ss >> token) {
		if (token == "wtime")
			ss >> wtime;
		else if (token == "btime")
			ss >> btime;
		else if (token == "winc")
			ss >> winc;
		else if (token == "binc")
			ss >> binc;
		else if (token == "movestogo")
			ss >> movestogo;
		else if (token == "movetime")
			ss >> info.timeLimit;
		else if (token == "depth")
			ss >> info.depthLimit;
		else if (token == "nodes")
			ss >> info.nodesLimit;
	}
	int time = pos.color == WHITE ? wtime : btime;
	int inc = pos.color == WHITE ? winc : binc;
	if (time)
		info.timeLimit = std::min(time / movestogo + inc, time / 2);
	IncrementHashDate();
	SearchIterate(pos);
}

static void UciCommand(string command) {
	if (command == "uci")
		cout << "id name " << NAME << endl << "uciok" << endl << flush;
	else if (command == "isready")
		cout << "readyok" << endl << flush;
	else if (command == "ucinewgame")
		TTClear();
	else if (command == "quit")
		exit(0);
	else if (command.substr(0, 8) == "position")
		ParsePosition(command);
	else if (command.substr(0, 2) == "go")
		ParseGo(command);
}

static void UciLoop() {
	//pos.SetFen("2k5/P2R4/4Q3/8/7p/3P1K1P/1PP2P2/8 w - - 35 107");
	//pos.SetFen("r1bqk1nr/pppnbppp/3p4/8/2BNP3/8/PPP2PPP/RNBQK2R w KQkq - 2 6");
	//UciCommand("go movetime 5000");
	string line;
	while (true) {
		getline(cin, line);
		UciCommand(line);
	}
}

int main() {
	cout << NAME << " " << VERSION << endl;
	setvbuf(stdout, NULL, _IONBF, 0);
	setvbuf(stdin, NULL, _IONBF, 0);
	fflush(NULL);
	InitializeBitboards();
	Position::StaticInitialize();
	InitializeEvaluation();
	InitializeHash(64);
	pos.SetFen(START_FEN);
	UciLoop();
	return 0;
}