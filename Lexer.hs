module Lexer where

import Text.ParserCombinators.Parsec

import Tokens

readTokens :: String -> String
readTokens input = case parse parseTokens "js" input of
    Left err -> "No match: " ++ show err
    Right value -> "Found value: " ++ show value

parseTokens :: Parser [Token]
parseTokens = spaces >> return []
