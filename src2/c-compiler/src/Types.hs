module Types where


bork :: IO ()
bork = putStrLn "bork"

data Token = OpenBrace 
    | CloseBrace 
    | OpenParen 
    | CloseParen 
    | Semicolon
    | IntKeyword 
    | ReturnKeyword
    | IntegerLiteral Int
    | Identifier String
    | Error

