ProgramNode parse_program(std::vector<Token>);
std::pair<FunctionNode, int> parse_function(std::vector<Token>);
std::pair<ExprNode, int> parse_expr(std::vector<Token>);
std::pair<StatementNode, int> parse_statement(std::vector<Token>);
std::pair<UnaryOpNode, int> parse_unary_op(std::vector<Token>);