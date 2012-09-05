module Lexer where

import Text.ParserCombinators.Parsec

import Tokens
import NullLiteral
import BooleanLiteral
import Keyword
import StringLiteral

readTokens :: String -> String
readTokens input = case parse parseTokens "js" input of
    Left err -> "No match: " ++ show err
    Right value -> "Found value: " ++ show value

parseTokens :: Parser [Token]
parseTokens = spaces >> endBy parseToken spaces

parseToken :: Parser Token
parseToken = 
    nullLiteral
    <|> booleanLiteral
    <|> keyword
    <|> stringLiteral