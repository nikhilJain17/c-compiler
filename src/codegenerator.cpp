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
        std::cout << " func " << std::endl;
    
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
        generated_code += generate_expr(s.get_expr());
        generated_code += "\tret\n";
    }
    return generated_code;
}

std::string generate_expr(ExprNode e) {

    std::string generated_code = "";
    if (e.get_type() == ExprType::constant) {
        generated_code += "\tmovl\t$";
        generated_code += e.get_data();
        generated_code += ", %EAX\n";
        return generated_code;
    }
    else if (e.get_type() == ExprType::unary_op) {
        UnaryOpNode unop = e.get_unop();
        // recurse on the inner expression
        generated_code += generate_expr(*e.get_unop_expr()) + "\n";

        if (unop.get_operator_type() == OperatorType::bitwise_complement) {
            // complement it
            generated_code += "\tnot\t%EAX\n";
        }
        else if (unop.get_operator_type() == OperatorType::negation) {
            // movl    $3, %eax    ;EAX register contains 3
            // neg     %eax
   
            // negate it
            generated_code += "\tneg\t%EAX";
        } 
        else if (unop.get_operator_type() == OperatorType::logical_negation) {
            // <CODE FOR exp GOES HERE>
            // cmpl   $0, %eax    ;set ZF on if exp == 0, set it off otherwise
            // movl   $0, %eax    ;zero out EAX (doesn't change FLAGS)
            // sete   %al         ;set AL register (the lower byte of EAX) to 1 iff ZF is on

            generated_code += generate_expr(*e.get_unop_expr()) + "\n";

            generated_code += "\tcmpl\t$0, %eax\n";
            generated_code += "\tmovl\t$0, %eax\n";
            generated_code += "\tsete\t%al\n";

        }
        return generated_code;
    }
    return generated_code;
}