#include "../src/seapiler.h"
#include "../src/lexer.h"
#include "../src/parser.h"

void test_lex_singletons() {
    std::optional<Token> a1 = lex_token("123");
    assert(a1.has_value());
    assert(a1.value().get_data().compare("123") == 0);
    assert(a1.value().get_type_str().compare("integer_literal") == 0);
    std::cout << "\t" << a1.value().token_to_string() << std::endl;

    std::optional<Token> a2 = lex_token("a123ajklsf09");
    assert(a2.has_value());
    assert(a2.value().get_data().compare("a123ajklsf09") == 0);
    assert(a2.value().get_type_str().compare("identifier") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;

    a2 = lex_token("1abc");
    assert(!a2.has_value());
    std::cout << "\t" << "1abc invalid token" << std::endl;

    a2 = lex_token("{");
    assert(a2.has_value());
    assert(a2.value().get_data().compare("{") == 0);
    assert(a2.value().get_type_str().compare("open_brace") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;
    
    a2 = lex_token("}");
    assert(a2.has_value());
    assert(a2.value().get_data().compare("}") == 0);
    assert(a2.value().get_type_str().compare("close_brace") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;
    
    a2 = lex_token("(");
    assert(a2.has_value());
    assert(a2.value().get_data().compare("(") == 0);
    assert(a2.value().get_type_str().compare("open_paren") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;
    
    a2 = lex_token(")");
    assert(a2.has_value());
    assert(a2.value().get_data().compare(")") == 0);
    assert(a2.value().get_type_str().compare("close_paren") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;
    
    a2 = lex_token(";");
    assert(a2.has_value());
    assert(a2.value().get_data().compare(";") == 0);
    assert(a2.value().get_type_str().compare("semicolon") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;
    
    a2 = lex_token("int");
    assert(a2.has_value());
    assert(a2.value().get_data().compare("int") == 0);
    assert(a2.value().get_type_str().compare("type_keyword") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;
    
    a2 = lex_token("return");
    assert(a2.has_value());
    assert(a2.value().get_data().compare("return") == 0);
    assert(a2.value().get_type_str().compare("return_keyword") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;
    
    a2 = lex_token("-123");
    assert(!is_token_from_rawdata("-123"));
    assert(!a2.has_value());
    std::cout << "\t" << "-123 is not a valid literal" << std::endl;

    a2 = lex_token("bbad");
    assert(a2.has_value());
    assert(a2.value().get_data().compare("bbad") == 0);
    assert(a2.value().get_type_str().compare("identifier") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;

    a2 = lex_token("DsdfDF3");
    assert(a2.has_value());
    assert(a2.value().get_data().compare("DsdfDF3") == 0);
    assert(a2.value().get_type_str().compare("identifier") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;

    a2 = lex_token("zZ3");
    assert(a2.has_value());
    assert(a2.value().get_data().compare("zZ3") == 0);
    assert(a2.value().get_type_str().compare("identifier") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;

    a2 = lex_token("A0");
    assert(a2.has_value());
    assert(a2.value().get_data().compare("A0") == 0);
    assert(a2.value().get_type_str().compare("identifier") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;

    a2 = lex_token("x");
    assert(a2.has_value());
    assert(a2.value().get_data().compare("x") == 0);
    assert(a2.value().get_type_str().compare("identifier") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;

    a2 = lex_token("043");
    assert(a2.has_value());
    assert(a2.value().get_data().compare("043") == 0);
    assert(a2.value().get_type_str().compare("integer_literal") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;



}

void test_parser_simple() {
    // parses the following program:
    
    std::cout << "\tParsing the following program:"  << std::endl;
    std::cout << "\t.............................\n";
    std::cout << "\tint main() {\n\t\treturn 2;\n\t}" << std::endl;
    
    std::vector<Token> simple_program;
    simple_program.push_back(Token("int"));
    simple_program.push_back(Token("main"));
    simple_program.push_back(Token("("));
    simple_program.push_back(Token(")"));
    simple_program.push_back(Token("{"));
    simple_program.push_back(Token("return"));
    simple_program.push_back(Token("2"));
    simple_program.push_back(Token(";"));
    simple_program.push_back(Token("}"));

    // should not throw any exceptions
    try {
        parseProgram(simple_program);
    } 
    catch (std::exception e) {
        std::cout << "Failed parsing: " << e.what() << std::endl;
    }

    std::cout << "\t" << std::endl;

}

void bork() {
    throw 2;
}

int main() {
    // clang++ -std=c++17 -stdlib=libc++ src/*.cpp tests/lexer_unit_tests.cpp -o lextest

    std::cout << "====================================" << std::endl << "\tLexer Tests\n" 
        << "====================================\n" << std::endl;

    std::cout << "------------------------------------\n";
    std::cout << "(Lexer Singleton Tests):\n" ;
    test_lex_singletons();
    std::cout << "Passed!" << std::endl;
    std::cout << "------------------------------------\n";

    std::cout << "(Parser Simple Tests):\n" ;
    test_parser_simple();
    std::cout << "Passed!" << std::endl;
    std::cout << "------------------------------------\n";

    return 0;
}