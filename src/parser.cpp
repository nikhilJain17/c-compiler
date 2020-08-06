#include "seapiler.h"
#include "parser.h"

ProgramNode parseProgram(std::vector<Token> tokens) {
    FunctionNode func = parseFunction(tokens);
    return ProgramNode(func);
}
FunctionNode parseFunction(std::vector<Token> tokens) {
    std::vector<Token>::iterator iter = tokens.begin();
    Token tok = *iter;

    if (tok.get_type() != TokenType::type_keyword) {
        std::cout << "Expected type keyword but got " << tok.get_type_str()
            << ": " << tok.get_data() << std::endl;
        exit(1);
    }

    iter++;
    tok = *iter;

    std::string identifier;
    if (tok.get_type() != TokenType::identifier) {
        std::cout << "Expected identifier keyword but got " << tok.get_type_str()
            << ": " << tok.get_data() << std::endl;
        exit(1);
    }
    identifier = tok.get_data();

    iter++;
    tok = *iter;

    if (tok.get_type() != TokenType::open_paren) {
        std::cout << "Expected open_paren but got " << tok.get_type_str()
            << ": " << tok.get_data() << std::endl;
        exit(1);
    }
    
    iter++;
    tok = *iter;

    if (tok.get_type() != TokenType::close_paren) {
        std::cout << "Expected close_paren but got " << tok.get_type_str()
            << ": " << tok.get_data() << std::endl;
        exit(1);
    }

    iter++;
    tok = *iter;

    if (tok.get_type() != TokenType::open_brace) {
        std::cout << "Expected open_brace but got " << tok.get_type_str()
            << ": " << tok.get_data() << std::endl;
        exit(1);
    }

    iter++;

    std::pair<StatementNode, int> result = parseStatement(std::vector<Token>(iter, tokens.end()));
    StatementNode stmt = result.first;

    iter += (result.second + 1); // @todo is this +1 correct
    tok = *iter;

    if (tok.get_type() != TokenType::close_brace) {
        std::cout << "Expected close_brace but got " << tok.get_type_str()
            << " : " << tok.get_data() << std::endl;
        exit(1);
    }
    
    return FunctionNode(identifier, stmt);

}
std::pair<StatementNode, int> parseStatement(std::vector<Token> tokens) {
    int tokens_to_pop = 0;
    
    std::vector<Token>::iterator iter = tokens.begin();
    Token tok = *iter;

    if (tok.get_type() != TokenType::return_keyword) {
        std::cout << "Expected return_keyword but got " << tok.get_type_str()
            << ": " << tok.get_data() << std::endl;
        exit(1);
    }

    tokens_to_pop++;
    iter++;
    tok = *iter;
    std::pair<ExprNode, int> result = parseExpr(std::vector<Token>(iter, tokens.end()));
    ExprNode expr = result.first;

    tokens_to_pop += result.second;
    iter += result.second;
    tok = *iter;

    if (tok.get_type() != TokenType::semicolon) {
        std::cout << "Expected semicolon but got " << tok.get_type_str()
            << ": " << tok.get_data() << std::endl;
        exit(1);
    }

    StatementNode stmt(expr, true);
    return std::pair<StatementNode, int>(stmt, tokens_to_pop);
}
std::pair<ExprNode, int> parseExpr(std::vector<Token> tokens) {
    int tokens_to_pop = 0;

    std::vector<Token>::iterator iter = tokens.begin();
    Token tok = *iter;
    tokens_to_pop++;

    if (tok.get_type() != TokenType::integer_literal) {
        std::cout << "Expected integer_literal but got " << tok.get_type_str()
            << ": " << tok.get_data() << std::endl;
        exit(1);
    }

    return std::pair<ExprNode, int>(ExprNode(tok.get_data()), tokens_to_pop);
}














































// #include "seapiler.h"

// ExprNode parseExpr(std::vector<Token> tokens) {
//     std::vector<Token>::iterator iter = tokens.begin();
//     Token tok = (*iter);

//     if (tok.get_type() != TokenType::integer_literal) {
//         std::cout << "ko\n";
//         std::cout << "syntax error - expected integer literal but got " << tok.get_type_str() 
//             << ", " << tok.get_data() << std::endl;
//         exit(1);
//     }

//     return ExprNode(tok.get_data());
// }

// StatementNode parseStatement(std::vector<Token> tokens) {
//     std::vector<Token>::iterator iter = tokens.begin();
//     Token tok = (*iter);
    
//     if (tok.get_type() != TokenType::return_keyword) {
//         std::cout << "syntax error - expected return" << std::endl;
//         exit(1);
//     }

//     iter++;
//     tok = (*iter);

//     // if (tok.get_type() != TokenType::integer_literal) {
//     //     std::cout << "a";
//     //     std::cout << "syntax error - expected integer literal in statement but got " 
//     //         << tok.get_type_str() << ": " << tok.get_data() << std::endl;
//     //     exit(1);
//     // }

//     // iter++;
//     // tok = (*iter);

//     ExprNode expr;
//     try {
//         std::cout << "1" << std::endl;
//         expr = parseExpr(std::vector(iter, tokens.end() - 1));
//     } 
//     catch (std::exception e) {
//         std::cout << "Failed parseExpr: " << e.what() << std::endl;
//     }

//     StatementNode stmt(expr, true);

//     iter++;
//     tok = (*iter);
//     if (tok.get_type() != TokenType::semicolon) {
//         std::cout << "syntax error - expected semicolon" << std::endl;
//         exit(1);
//     }

//     return stmt;
// }

// FunctionNode parseFunction(std::vector<Token> tokens) {
//     std::vector<Token>::iterator iter = tokens.begin();
//     Token tok = (*iter);

//     // function declaration :
//     // <type_keyword> <identifier> "(" ")" "{" <stmt> "}"

//     // @TODO pass in get_type not get_data dumboi
//     if (tok.get_type() != TokenType::type_keyword) {
//         std::cout << "b";
//         std::cout << "syntax error - expected type keyword in func declr but got " << tok.get_type() << std::endl;
//         exit(1);
//     }

//     iter++;
//     tok = (*iter);

//     if (tok.get_type() != TokenType::identifier) {
//         std::cout << "syntax error - expected identifiers" << std::endl;
//         exit(1);
//     }

//     std::string identifier = tok.get_data();

//     iter++;
//     tok = (*iter);

//     if (tok.get_type() != TokenType::open_paren) {
//         std::cout << "syntax error - expected open paren" << std::endl;
//         exit(1);
//     }

//     iter++;
//     tok = (*iter);

//     if (tok.get_type() != TokenType::close_paren) {
//         std::cout << "syntax error - expected close paren" << std::endl;
//         exit(1);
//     }

//     iter++;
//     tok = (*iter);

//     if (tok.get_type() != TokenType::open_brace) {
//         std::cout << "syntax error - expected open brace" << std::endl;
//         exit(1);
//     }

//     iter++;
//     tok = (*iter);

//     StatementNode body;
//     try {
//         std::cout << "2" << std::endl;
//         body = parseStatement(std::vector(iter, tokens.end()));
//     } 
//     catch (std::exception e) {
//         std::cout << "Failed parseExpr: " << e.what() << std::endl;
//     }

//     iter = tokens.end();
//     tok = (*iter);

//     if (tok.get_type() != TokenType::close_brace) {
//         std::cout << "syntax error - expected close brace but got " << 
//             tok.get_type_str() << " : " << tok.get_data() << std::endl;
//         exit(1);
//     }
    
//     return FunctionNode(identifier, body);

// }

// ProgramNode parseProgram(std::vector<Token> tokens) {
//     ProgramNode p;
//     try {
//         p = parseFunction(tokens);
//     } 
//     catch (std::exception e) {
//         std::cout << "Failed parseExpr: " << e.what() << std::endl;
//     }
//     return p;
// }


