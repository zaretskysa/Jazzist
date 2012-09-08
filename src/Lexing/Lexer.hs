module Lexing.Lexer where

import Text.ParserCombinators.Parsec

import Lexing.Tokens
import Lexing.NullLiteral
import Lexing.BooleanLiteral
import Lexing.Keyword
import Lexing.StringLiteral
import Lexing.NumericLiteral
import Lexing.Comment
import Lexing.Identifier
import Lexing.Punctuator

readTokens :: String -> String
readTokens input = case parse parseTokens "js" input of
    Left err -> "No match: " ++ show err
    Right value -> "Found value: " ++ show value

parseTokens :: Parser [Token]
parseTokens = spaces >> endBy parseToken spaces

parseToken :: Parser Token
parseToken = 
    identifier
    <|> nullLiteral
    <|> booleanLiteral
    <|> keyword
    <|> stringLiteral
    <|> numericLiteral
    <|> try comment
    <|> punctuator
