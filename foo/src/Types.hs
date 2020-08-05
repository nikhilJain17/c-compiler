module Types where


bork :: IO ()
bork = putStrLn "bork"

data Token = 
    OpenBrace 
    | CloseBrace 
    | OpenParen 
    | CloseParen 
    | Semicolon
    | IntKeyword 
    | ReturnKeyword
    | IntegerLiteral Int
    | Identifier String
    | ErrorToken

data ASTNode =
    ProgramNode Program 
    | FuncNode FunctionDeclaration 
    | StatementNode Statement 
    | ExpressionNode Expr
    | ReturnNode
    | ErrorNode
    | Nil

data AST = AST {
    root :: ASTNode,
    children :: [AST] -- a child could be nil
}

data Program = FunctionDeclaration

data FunctionDeclaration = Func {
        name :: String,
        body :: Statement
    }

data Statement = ReturnVal Expr

data Expr = Const Int