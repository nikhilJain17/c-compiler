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
    | Error

data ASTNode =
    ProgramNode Program 
    | FuncNode FunctionDeclaration 
    | StatementNode Statement 
    | ExpressionNode Expr

data AST = AST {
    root :: ASTNode,
    left :: Maybe ASTNode, -- a child could be nil
    right :: Maybe ASTNode
}

data Program = FunctionDeclaration

data FunctionDeclaration = Func {
        name :: String,
        body :: Statement
    }

data Statement = ReturnVal Expr

data Expr = Const Int