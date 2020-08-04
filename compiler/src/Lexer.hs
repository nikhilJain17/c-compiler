module Lexer
( 
    someFunc
) where

import Types
import Data.Char

someFunc :: IO ()
someFunc = bork

lexFile :: String -> [Token]
lexFile filename = undefined

lexSingleton :: String -> Maybe Token
lexSingleton "{" = Just OpenBrace
lexSingleton "}" = Just CloseBrace
lexSingleton "(" = Just OpenParen
lexSingleton ")" = Just CloseParen
lexSingleton ";" = Just Semicolon
lexSingleton "int" = Just IntKeyword
lexSingleton "return" = Just ReturnKeyword
lexSingleton (a:as)
    | (isInteger (a:as)) && (a /= '-') = Just $ IntegerLiteral (read (a:as))
    | (isAlphaNum a) && (isInteger as) = Just $ Identifier (a:as)
    where
        isInteger s = case reads s :: [(Integer, String)] of
                [(_, "")] -> True
                _         -> False
lexSingleton _ = Nothing
                  
