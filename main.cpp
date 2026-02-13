#include "main.h"

using namespace std;

SearchInfo info;
HashEntry* tt = 0;
U64 ttMask = 0;
int ttDate = 0;
Position pos;

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

void IncrementHashDate(){
	ttDate = (ttDate + 1) & 0xf;
}

static std::string ReadLine() {
	const int size = 16000;
	char line[size] = {};
	std::fgets(line, size, stdin);
	return std::string(line);
}

void ResetInfo() {
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
	if (GetMoveType(move) == MoveTypePromotion)
	{
		switch (GetPromotionMoveType(move))
		{
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

void UciCommand(string command) {
	if (command.empty())
		return;
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