module TestUtils where

import Text.ParserCombinators.Parsec

import Lexing.Token

parseWholeInput :: Parser a -> Parser a
parseWholeInput parser = do
	token <- parser
	eof
	return token

-- TODO: remove this version
parseTestInput :: Parser Token -> String -> Maybe Token
parseTestInput parser input = case parse parser "testInput" input of
    Left err -> Nothing
    Right res -> Just res

parseWholeTestInput :: Parser a -> String -> Maybe a
parseWholeTestInput parser input = case parse (parseWholeInput parser) "testInput" input of
    Left err -> Nothing
    Right res -> Just res