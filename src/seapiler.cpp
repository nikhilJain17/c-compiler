#include "seapiler.h"

void compile()
{
    // clang++ -std=c++17 -stdlib=libc++ seapiler.cpp lexer.cpp
}

const std::string Token::type_map[] = {"open_brace","close_brace","open_paren",
    "close_paren","semicolon","type_keyword","return_keyword",
    "integer_literal","identifier"};


Token::Token(std::string data)
{
    this->data = data;

    // tediously match on type...this is one thing that would unironically be easier in Haskell
    if (is_open_brace_from_rawdata(data)) {
        this->type = open_brace;
    }
    else if (is_close_brace_from_rawdata(data)) {
        this->type = close_brace;
    } 
    else if (is_open_paren_from_rawdata(data)) {
        this->type = open_paren;
    } 
    else if (is_close_paren_from_rawdata(data)) {
        this->type = close_paren;
    } 
    else if (is_semicolon_from_rawdata(data)) {
        this->type = semicolon;
    }
    else if (is_type_keyword_from_rawdata(data)) {
        this->type = type_keyword;
    }
    else if (is_return_keyword_from_rawdata(data)) {
        this->type = return_keyword;
    }
    else if (is_integer_literal_from_rawdata(data)) {
        this->type = integer_literal;
    }
    else if (is_identifier_from_rawdata(data)) {
        this->type = identifier;
    } 
    else {
        // should technically never reach here...
        throw std::invalid_argument("cannot tokenize -- something is very broken");
    }
    
}

// std::string Token::get_type const () {
//     return Token::type_map[this->type];
// }

// std::string Token::get_data const () {
//     return this->data;
// }

// overload printing for Tokens
std::string Token::token_to_string()
{ 
    std::string s = "< ";
    s.append(this->get_type_str());
    s.append(" , ");
    s.append(this->get_data());
    s.append(" >");
    return s;            
}

Token::~Token()
{
}

// @TODO refactor the "get type stuff"
bool is_open_brace_from_rawdata(std::string data) {
    return data.compare("{") == 0;
}
bool is_close_brace_from_rawdata(std::string data) {
    return data.compare("}") == 0;
}
bool is_open_paren_from_rawdata(std::string data) {
    return data.compare("(") == 0;
}
bool is_close_paren_from_rawdata(std::string data) {
    return data.compare(")") == 0;
}
bool is_semicolon_from_rawdata(std::string data) {
    return data.compare(";") == 0;
}
bool is_type_keyword_from_rawdata(std::string data) {
    return data.compare("int") == 0;// || (data.compare("bool") == 0));
}
bool is_return_keyword_from_rawdata(std::string data) {
    return (data.compare("return") == 0);
}
bool is_integer_literal_from_rawdata(std::string data) {
    // only positive literals are allowed
    if (data[0] == '-') {
        return false;
    }
    for (int i = 0; i < data.length(); i++) {
        if (!isdigit(data[i])) {
            return false;
        }
    }
    return true;
}
bool is_identifier_from_rawdata(std::string data) {
    // first letter has to be alpha
    if (!isalpha(data[0])) {
        return false;
    }
    // rest of the letters can be alphanumeric
    for (int i = 1; i < data.length(); i++) {
        if (!(isalpha(data[i]) || isdigit(data[i]))) {
            return false;
        }
    }
    return true;
}

bool is_token_from_rawdata(std::string tok) {
    
    if (tok.length() == 0) {
        return false;
    }

    return is_open_brace_from_rawdata(tok) || is_close_brace_from_rawdata(tok) || is_open_paren_from_rawdata(tok)
        || is_close_paren_from_rawdata(tok) || is_semicolon_from_rawdata(tok) || is_type_keyword_from_rawdata(tok)
        || is_return_keyword_from_rawdata(tok) || is_integer_literal_from_rawdata(tok) || is_identifier_from_rawdata(tok);
}
