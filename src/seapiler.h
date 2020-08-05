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
    std::string data;
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


class FunctionNode : public ASTNode
{
private:
    std::string identifier;
    ExprNode expression;
public:
    FunctionNode() = default;
    FunctionNode(std::string id, ExprNode expr) {
        this->identifier = id;
        this->expression = expr;
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