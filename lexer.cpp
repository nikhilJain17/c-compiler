#include "seapiler.h"
#include "lexer.h"

/*
    Lex a whole file and return a list of tokens.
*/
std::vector<Token> lex_file(std::string filename) {

    // @TODO validate filename
    std::ifstream filestream(filename);
    if (!filestream.is_open()) {
        throw std::ios_base::failure("could not open file");
    }

    std::vector<Token> token_list;
    std::string curr_tok;
    while (filestream >> curr_tok) {
        std::optional<Token> tok_result = lex_token(curr_tok);
        if (tok_result.has_value()) {
            token_list.push_back(tok_result.value());
        }
    }

    return token_list; 
}

/*
    Lex a single word into a token
*/
std::optional<Token> lex_token(std::string tok) {
    if (is_token(tok)) {
        return std::optional<Token>(Token(tok));
    }
    else {
        return std::optional<Token> (); 
    }
}
