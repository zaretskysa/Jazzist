module TestUtils where
--TODO: rename to LexingUtils

import Text.ParserCombinators.Parsec

import Lexing.Token

parseWholeInput :: Parser a -> Parser a
parseWholeInput parser = do
    token <- parser
    eof
    return token

parseWholeTestInput :: Parser a -> String -> Maybe a
parseWholeTestInput parser input = case parse (parseWholeInput parser) "testInput" input of
    Left err -> Nothing
    Right res -> Just res
