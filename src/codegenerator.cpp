#include "seapiler.h"
#include "codegenerator.h"

std::string generate(ProgramNode root) {
    // std::string generated_code = "";
    // assume its program node for now
    return generate_function(root.get_function_node());
}

std::string generate_function(FunctionNode f) {
    /*
    .globl _foo
    _foo:
        <body>
    */
    std::string generated_code = ".globl _";
    generated_code += f.get_identifier();
    generated_code += "\n";
    generated_code += "_";
    generated_code += f.get_identifier();
    generated_code += ":\n";
    StatementNode s = f.get_function_body();
    generated_code += generate_statement(f.get_function_body());

    return generated_code;
}

std::string generate_statement(StatementNode s) {
    std::string generated_code = "";
    if (s.get_statement_type().compare("return_stmt") == 0) {
        generated_code += "\tmovl\t$";
        generated_code += s.get_expr().get_data();
        generated_code += ", %EAX\n";
        generated_code += "\tret\n";
    }
    return generated_code;
}