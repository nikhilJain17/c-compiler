// (All our includes go here), as well as all shared types, functions, etc
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <iostream>
#include <optional>

enum TokenType {
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

class Token
{
private:
    std::string data;
    TokenType type;

public:
    Token(std::string data);
    ~Token();
    std::string get_type_str() const {
        return type_map[type];
    }
    std::string get_data() const {
        return data;
    }
    const static std::string type_map[];
    std::string token_to_string();
    TokenType get_type() {
        return this->type;
    };
};

// raw code --> type
bool is_open_brace_from_rawdata(std::string);
bool is_close_brace_from_rawdata(std::string);
bool is_open_paren_from_rawdata(std::string);
bool is_close_paren_from_rawdata(std::string);
bool is_semicolon_from_rawdata(std::string);
bool is_type_keyword_from_rawdata(std::string);
bool is_return_keyword_from_rawdata(std::string);
bool is_integer_literal_from_rawdata(std::string);
bool is_identifier_from_rawdata(std::string);
bool is_token_from_rawdata(std::string);

class ASTNode
{
private:
    std::vector<ASTNode> children;
public:
};

class ExprNode : public ASTNode
{
private:
    std::string value;
public:
    ExprNode() = default;
    ExprNode(std::string s) {
        this->value = s;
    }
};

class StatementNode : public ASTNode
{
private:
    enum Type {
        return_stmt,
        nil
    };
    Type type;
    ExprNode expr;
public:
    StatementNode() = default;
    // return statement constructor
    StatementNode(ExprNode e, bool is_return_statement=false) {
        if (is_return_statement) {
            this->type = return_stmt;
            this->expr = e;
        }
    }
};

class FunctionNode : public ASTNode
{
private:
    std::string identifier;
    StatementNode body;
public:
    FunctionNode() = default;
    FunctionNode(std::string id, StatementNode b) {
        this->identifier = id;
        this->body = b;
    }
};

class ProgramNode : public ASTNode
{
private:
    FunctionNode function_child;
public:
    ProgramNode() = default;
    ProgramNode(FunctionNode f) {
        this->function_child = f;
    }
};
