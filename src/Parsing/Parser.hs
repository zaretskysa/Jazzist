module Parsing.Parser
(
    module Parsing.Ast,

    parseString
) where

import Text.ParserCombinators.Parsec (ParseError)

import Parsing.Ast
import Lexing.Token
import Lexing.Lexer (tokenize)
import Parsing.ProgramParser

parseString :: String -> Either ParseError Program
parseString input = case tokenize input of
    Left err -> Left err
    Right toks -> parseTokens toks

parseTokens :: [Token] -> Either ParseError Program
parseTokens input = parse program "JsParser" input
