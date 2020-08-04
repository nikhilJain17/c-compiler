#include "../src/seapiler.h"
#include "../src/lexer.h"

void test_lex_singletons() {
    std::optional<Token> a1 = lex_token("123");
    assert(a1.has_value());
    assert(a1.value().get_data().compare("123") == 0);
    assert(a1.value().get_type().compare("integer_literal") == 0);
    std::cout << "\t" << a1.value().token_to_string() << std::endl;

    std::optional<Token> a2 = lex_token("a123ajklsf09");
    assert(a2.has_value());
    assert(a2.value().get_data().compare("a123ajklsf09") == 0);
    assert(a2.value().get_type().compare("identifier") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;

    a2 = lex_token("1abc");
    assert(!a2.has_value());
    std::cout << "\t" << "1abc invalid token" << std::endl;

    a2 = lex_token("{");
    assert(a2.has_value());
    assert(a2.value().get_data().compare("{") == 0);
    assert(a2.value().get_type().compare("open_brace") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;
    
    a2 = lex_token("}");
    assert(a2.has_value());
    assert(a2.value().get_data().compare("}") == 0);
    assert(a2.value().get_type().compare("close_brace") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;
    
    a2 = lex_token("(");
    assert(a2.has_value());
    assert(a2.value().get_data().compare("(") == 0);
    assert(a2.value().get_type().compare("open_paren") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;
    
    a2 = lex_token(")");
    assert(a2.has_value());
    assert(a2.value().get_data().compare(")") == 0);
    assert(a2.value().get_type().compare("close_paren") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;
    
    a2 = lex_token(";");
    assert(a2.has_value());
    assert(a2.value().get_data().compare(";") == 0);
    assert(a2.value().get_type().compare("semicolon") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;
    
    a2 = lex_token("int");
    assert(a2.has_value());
    assert(a2.value().get_data().compare("int") == 0);
    assert(a2.value().get_type().compare("type_keyword") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;
    
    a2 = lex_token("return");
    assert(a2.has_value());
    assert(a2.value().get_data().compare("return") == 0);
    assert(a2.value().get_type().compare("return_keyword") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;
    
    a2 = lex_token("-123");
    assert(!is_token("-123"));
    assert(!a2.has_value());
    std::cout << "\t" << "-123 is not a valid literal" << std::endl;

    a2 = lex_token("bbad");
    assert(a2.has_value());
    assert(a2.value().get_data().compare("bbad") == 0);
    assert(a2.value().get_type().compare("identifier") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;

    a2 = lex_token("DsdfDF3");
    assert(a2.has_value());
    assert(a2.value().get_data().compare("DsdfDF3") == 0);
    assert(a2.value().get_type().compare("identifier") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;

    a2 = lex_token("zZ3");
    assert(a2.has_value());
    assert(a2.value().get_data().compare("zZ3") == 0);
    assert(a2.value().get_type().compare("identifier") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;

    a2 = lex_token("A0");
    assert(a2.has_value());
    assert(a2.value().get_data().compare("A0") == 0);
    assert(a2.value().get_type().compare("identifier") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;

    a2 = lex_token("x");
    assert(a2.has_value());
    assert(a2.value().get_data().compare("x") == 0);
    assert(a2.value().get_type().compare("identifier") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;

    a2 = lex_token("043");
    assert(a2.has_value());
    assert(a2.value().get_data().compare("043") == 0);
    assert(a2.value().get_type().compare("integer_literal") == 0);
    std::cout << "\t" << a2.value().token_to_string() << std::endl;



}

int main() {
    std::cout << "==================" << std::endl << "   Lexer Tests\n" 
        << "==================\n" << std::endl;

    std::cout << "----------------------\n";
    std::cout << "(Lexer Singleton Tests):\n" ;
    test_lex_singletons();
    std::cout << "Passed!" << std::endl;
    std::cout << "----------------------\n";

    return 0;
}