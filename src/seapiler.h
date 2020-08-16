// (All our includes go here), as well as all shared types, functions, etc
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <iostream>
#include <optional>

void compile();
// int main();

enum TokenType {
    open_brace,
    close_brace,
    open_paren,
    close_paren,
    semicolon,
    type_keyword,
    return_keyword,
    integer_literal,
    identifier,
    negation_op,
    bitwise_complement_op,
    logical_negation_op
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
bool is_negation_op_from_rawdata(std::string);
bool is_bitwise_complement_op_from_rawdata(std::string);
bool is_logical_negation_op_from_rawdata(std::string);

// class ASTNode;
// class FunctionNode;
// class ExprNode;
// class StatementNode;
// class ProgramNode;
// class UnaryOpNode;


/*
    AST types
*/

class ASTNode
{
protected:
    std::vector<ASTNode> children;
public:
    std::vector<ASTNode> get_children() {
        return this->children;
    }
};

enum OperatorType 
{
    negation,
    bitwise_complement,
    logical_negation
};
class UnaryOpNode : public ASTNode 
{
private:
    OperatorType op_type;
    std::string data;
public:
    UnaryOpNode() = default;
    UnaryOpNode(std::string data) {
        this->data = data;
        if (data.compare("!") == 0) {
            this->op_type = OperatorType::logical_negation;
        }
        else if (data.compare("~") == 0) {
            this->op_type = OperatorType::bitwise_complement;
        }
        else if (data.compare("-") == 0) {
            this->op_type = OperatorType::negation;
        }
        else {
            throw new std::invalid_argument("expected unary op but got " +  data);
        }
    }
    OperatorType get_operator_type() {
        return this->op_type;
    }
    std::string get_data() {
        return this->data;
    }
};


enum ExprType 
{
    constant,
    unary_op
};

class ExprNode : public ASTNode
{
// <exp> ::= <unary_op> <exp> | <int>
private:
    std::string data;
    ExprType type;
    UnaryOpNode unop;
    ExprNode *unop_expr; // since recursive types are not allowed in c++
public:
    ExprNode() = default;
    ExprNode(std::string s, ExprType t, UnaryOpNode unop = UnaryOpNode(), ExprNode* unop_expr = NULL) {
        this->type = t;
        this->data = s;
        this->unop = unop;
        this->unop_expr = unop_expr;
    }
    ExprNode(const ExprNode &e) { // copy constructor
        this->type = e.type;
        this->data = e.data;
        this->unop = e.unop;
        this->unop_expr = e.unop_expr;
        // if (e.child != NULL) {
        //     this->child = e.child;
        //     std::cout << "agnr\n";

        // }

    }
    std::string get_data() {
        return this->data;
    }
    ExprType get_type() {
        return this->type;
    }
    UnaryOpNode get_unop() {
        return this->unop;
    }
    ExprNode* get_unop_expr() {
        return this->unop_expr;
    }
};

enum StatementType {
    return_stmt,
    nil
};
class StatementNode : public ASTNode
{
private:
    StatementType type;
    ExprNode expr;
public:
    StatementNode() = default;
    StatementNode(ExprNode e, StatementType type) {
        if (type == StatementType::return_stmt) {
            this->type = StatementType::return_stmt;
            this->expr = e;
            // children.push_back(e);
        }
    }
    std::string get_statement_type() {
        switch (this->type)
        {
        case StatementType::return_stmt:
            return "return_stmt";
        
        default:
            return "nil";
        }
    }
    ExprNode get_expr() {
        return this->expr;
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
        // children.push_back(b);
    }
    std::string get_identifier() {
        return this->identifier;
    }
    StatementNode get_function_body() {
        return this->body;
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
        // children.push_back(f);
    }
    FunctionNode get_function_node() {
        return this->function_child;
    }
};
