module Parsing.Parser where

import Text.ParserCombinators.Parsec

import Parsing.Data.Program
import Lexing.Tokens
import Lexing.Lexer (tokenize)
import Parsing.TokenParser
import Parsing.Data.Program
import Parsing.Parsers.Program

tryToParseString :: String -> String
tryToParseString input = case parseString input of
    Left err -> "No match: " ++ show err
    Right value -> "Found value: " ++ show value

parseString :: String -> Either ParseError Program
parseString input = case tokenize input of
    Left err -> Left err
    Right tokens -> parseTokens tokens

parseTokens :: [Token] -> Either ParseError Program
parseTokens input = parse program "JsParser" input
