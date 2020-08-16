#include "seapiler.h"
#include "parser.h"

/* Formal Grammar */
// <program> ::= <function>
// <function> ::= "int" <id> "(" ")" "{" <statement> "}"
// <statement> ::= "return" <exp> ";"
// <exp> ::= <unary_op> <exp> | <int>
// <unary_op> ::= "!" | "~" | "-"

ProgramNode parse_program(std::vector<Token> tokens) {
    FunctionNode main = parse_function(tokens).first;
    return ProgramNode(main);
}

std::pair<FunctionNode, int> parse_function(std::vector<Token> tokens) {
    std::vector<Token>::iterator iter = tokens.begin();
    Token curr_tok = *iter;
    int tokens_popped = 0;

    // "int"
    if (curr_tok.get_type() != TokenType::type_keyword) {
        throw std::invalid_argument("expected return type keyword but got " + curr_tok.token_to_string());
        exit(1);
    }

    // "<id>"
    tokens_popped++;
    iter++;
    curr_tok = *iter;

    if (curr_tok.get_type() != TokenType::identifier) {
        throw std::invalid_argument("expected function identifier but got " + curr_tok.token_to_string());
        exit(1);
    }
    std::string identifier = curr_tok.get_data();
    
    // "("
    tokens_popped++;
    iter++;
    curr_tok = *iter;
    
    if (curr_tok.get_type() != TokenType::open_paren) {
        throw std::invalid_argument("expected open paren but got " + curr_tok.token_to_string());
        exit(1);    
    }
      
    // ")"
    tokens_popped++;
    iter++;
    curr_tok = *iter;
    
    if (curr_tok.get_type() != TokenType::close_paren) {
        throw std::invalid_argument("expected close paren but got " + curr_tok.token_to_string());
        exit(1);
    }

    // "{"
    tokens_popped++;
    iter++;
    curr_tok = *iter;
    
    if (curr_tok.get_type() != TokenType::open_brace) {
        throw std::invalid_argument("expected open brace but got " + curr_tok.token_to_string());
        exit(1);
    }

    // <statement>
    tokens_popped++;
    iter++;
    std::pair<StatementNode, int> result = parse_statement(std::vector(iter, tokens.end()));
    StatementNode func_body = result.first;
    tokens_popped += result.second;
    iter += result.second;

    // }
    tokens_popped++;
    iter++;
    curr_tok = *iter;

    if (curr_tok.get_type() != TokenType::close_brace) {
        throw std::invalid_argument("expected close brace but got " + curr_tok.token_to_string());
        exit(1);    
    }

    FunctionNode func(identifier, func_body);
    return std::pair<FunctionNode, int>(func, tokens_popped);

}

std::pair<StatementNode, int> parse_statement(std::vector<Token> tokens) {
    std::vector<Token>::iterator iter = tokens.begin();
    Token curr_tok = *iter;
    int tokens_popped = 0;
    
    // "return"
    if (curr_tok.get_type() != TokenType::return_keyword) {
        throw std::invalid_argument("expected return keyword but got " + curr_tok.token_to_string());
        exit(1);
    }

    // <expr>
    tokens_popped++;
    iter++;
    std::pair<ExprNode, int> result = parse_expr(std::vector<Token>(iter, tokens.end()));
    ExprNode expr = result.first;

    // ";"
    iter += result.second;
    tokens_popped += result.second;
    curr_tok = *iter;
    if (curr_tok.get_type() != TokenType::semicolon) {
        throw std::invalid_argument("expected semicolon but got " + curr_tok.token_to_string());
        exit(1);
    }

    StatementNode stmt(expr, StatementType::return_stmt);
    return std::pair<StatementNode, int>(stmt, tokens_popped);
}

std::pair<ExprNode, int> parse_expr(std::vector<Token> tokens) {
    std::vector<Token>::iterator iter = tokens.begin();
    Token curr_tok = *iter;
    int tokens_popped = 0;

    if (curr_tok.get_type() == TokenType::logical_negation_op || 
        curr_tok.get_type() == TokenType::bitwise_complement_op || 
        curr_tok.get_type() == TokenType::negation_op) {
        // unary op

        std::pair<UnaryOpNode, int> result = parse_unary_op(std::vector<Token>(iter, tokens.end()));
        UnaryOpNode unop = result.first;
        tokens_popped += result.second;
        iter += result.second;

        std::pair<ExprNode, int> result2 = parse_expr(std::vector<Token>(iter, tokens.end()));
        ExprNode* body = new ExprNode(result2.first);
        tokens_popped += result2.second; 

        std::string val = unop.get_data() + body->get_data();
        ExprNode expr_node(val, ExprType::unary_op, unop, body);

        return std::pair<ExprNode, int>(expr_node, tokens_popped);

    }
    else if (curr_tok.get_type() == TokenType::integer_literal) {
        // const
        ExprNode expr(curr_tok.get_data(), ExprType::constant);
        tokens_popped++;
        return std::pair<ExprNode, int>(expr, tokens_popped);
    }
    else {
        std::invalid_argument("expected expr but got " + curr_tok.token_to_string());
        exit(1);
    }
}

std::pair<UnaryOpNode, int> parse_unary_op(std::vector<Token> tokens) {
    std::vector<Token>::iterator iter = tokens.begin();
    Token curr_tok = *iter;
    int tokens_popped = 0;

    UnaryOpNode unop(curr_tok.get_data());
    tokens_popped++;

    return std::pair<UnaryOpNode, int>(unop, tokens_popped);
}
