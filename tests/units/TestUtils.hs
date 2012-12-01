module TestUtils where

import Text.ParserCombinators.Parsec

import Lexing.Tokens

parseWholeInput :: Parser Token -> Parser Token
parseWholeInput parser = do
	token <- parser
	eof
	return token

-- TODO: remove this version
parseTestInput :: Parser Token -> String -> Maybe Token
parseTestInput parser input = case parse parser "testInput" input of
    Left err -> Nothing
    Right res -> Just res

parseWholeTestInput :: Parser Token -> String -> Maybe Token
parseWholeTestInput parser input = case parse (parseWholeInput parser) "testInput" input of
    Left err -> Nothing
    Right res -> Just res