FunctionNode parseFunction(std::vector<Token>);
ProgramNode parseProgram(std::vector<Token>);
std::pair<ExprNode, int> parseExpr(std::vector<Token>);
std::pair<StatementNode, int> parseStatement(std::vector<Token>);
