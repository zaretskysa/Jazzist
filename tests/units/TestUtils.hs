module TestUtils where

import Text.ParserCombinators.Parsec

import Lexing.Tokens

parseTestInput :: Parser Token -> String -> Maybe Token
parseTestInput parser input = case parse parser "testInput" input of
    Left err -> Nothing
    Right res -> Just res
