// (All our includes go here), as well as all shared types, functions, etc
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <iostream>
#include <optional>

class Token
{
private:
    enum Type {
        open_brace,
        close_brace,
        open_paren,
        close_paren,
        semicolon,
        type_keyword,
        return_keyword,
        integer_literal,
        identifier
    };
    Type type;
    std::string data;
public:
    Token(std::string data);
    ~Token();
    std::string get_type() const {
        return type_map[type];
    }

    std::string get_data() const {
        return data;
    }
    const static std::string type_map[];
    std::string token_to_string();
};

bool is_open_brace(std::string);
bool is_close_brace(std::string);
bool is_open_paren(std::string);
bool is_close_paren(std::string);
bool is_semicolon(std::string);
bool is_type_keyword(std::string);
bool is_return_keyword(std::string);
bool is_integer_literal(std::string);
bool is_identifier(std::string);
bool is_token(std::string);
