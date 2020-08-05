#include "seapiler.h"

ExprNode parseExpr(std::vector<Token> tokens) {
    std::vector<Token>::iterator iter = tokens.begin();
    Token tok = (*iter);

    if (!is_integer_literal(tok.get_type())) {
        throw new std::invalid_argument("syntax error - expected integer literal");
    }

    return ExprNode(tok.get_data());
}

StatementNode parseStatement(std::vector<Token> tokens) {
    std::vector<Token>::iterator iter = tokens.begin();
    Token tok = (*iter);
    
    if (!is_return_keyword(tok.get_type())) {
        throw std::invalid_argument("syntax error while parsing statement");
    }

    iter++;
    tok = (*iter);
    if (!is_type_keyword(tok.get_type())) {
        throw std::invalid_argument("syntax error while parsing statement");
    }

    ExprNode expr = parseExpr(std::vector(iter, tokens.end() - 1));
    StatementNode stmt(expr, true);

    iter++;
    tok = (*iter);
    if (!is_semicolon(tok.get_type())) {
        throw std::invalid_argument("syntax error while parsing statement -- missing semicolon");
    }

    return stmt;
}

FunctionNode parseFunction(std::vector<Token> tokens) {
    std::vector<Token>::iterator iter = tokens.begin();
    Token tok = (*iter);

    // function declaration :
    // <type_keyword> <identifier> "(" ")" "{" <stmt> "}"

    if (!is_type_keyword(tok.get_type())) {
        throw new std::invalid_argument("syntax error - expected type keyword");
    }

    iter++;
    tok = (*iter);

    if (!is_identifier(tok.get_type())) {
        throw new std::invalid_argument("syntax error - expected identifier");
    }

    std::string identifier = tok.get_data();

    iter++;
    tok = (*iter);

    if (!is_open_paren(tok.get_type())) {
        throw new std::invalid_argument("syntax error - expected open parenthesis");
    }

    iter++;
    tok = (*iter);

    if (!is_close_paren(tok.get_type())) {
        throw new std::invalid_argument("syntax error - expected close parenthesis");
    }

    iter++;
    tok = (*iter);

    if (!is_open_brace(tok.get_type())) {
        throw new std::invalid_argument("syntax error - expected open brace");
    }

    ExprNode expr = parseExpr(std::vector(iter, tokens.end()));

    iter++;
    tok = (*iter);

    if (!is_close_brace(tok.get_type())) {
        throw new std::invalid_argument("syntax error - expected close brace");
    }
    
    return FunctionNode(identifier, expr);

}

ProgramNode parseProgram(std::vector<Token> tokens) {
    return parseFunction(tokens);
}


