module Parser where

import Types

parse :: [Token] -> AST
parse (x:xs) = undefined

parseFunction :: [Token] -> AST
parseFunction (x:xs) 
    | isValid x = undefined -- FuncNode (FunctionDeclaration identifier statement)
    | otherwise = AST ErrorNode []
        where
            isValid :: Token -> Bool 
            isValid IntKeyword = True
            isValid _ = False

            identifier :: Token -> String
            identifier (Identifier s) = s
            identifier _ = undefined -- todo throw error

            statement :: Statement
            statement = undefined

-- func = intkeyword identifier openparen closeparen openbrace 
parseStatement :: [Token] -> AST
parseStatement (x:y:xs) 
    | (isValidStatement x y) = AST (ReturnNode) [(parseExpr xs)]
    | otherwise = (AST ErrorNode [])
    where
        isValidStatement :: Token -> Token -> Bool
        isValidStatement ReturnKeyword IntKeyword = True -- @todo check for semicolon
        isValidStatement _ _ = False

parseExpr :: [Token] -> AST
parseExpr [IntegerLiteral x, Semicolon] = AST (ExpressionNode (Const x)) []