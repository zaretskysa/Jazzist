module Parsing.Parser
(
    module Parsing.Ast,

    parseString,
    primaryExpressionFromString
) where

import Text.ParserCombinators.Parsec (ParseError)

import Parsing.Ast
import Lexing.Token
import Lexing.Lexer (tokenize)
import Parsing.ProgramParser

--TODO: unify function names

parseString :: String -> Either ParseError Program
parseString input = case tokenize input of
    Left err -> Left err
    Right toks -> parseTokens toks

parseTokens :: [Token] -> Either ParseError Program
parseTokens input = parse program "JsParser" input

primaryExpressionFromString :: String -> Either ParseError PrimaryExpression
primaryExpressionFromString input = case tokenize input of
    Left err -> Left err
    Right toks -> primaryExpressionFromTokens toks

primaryExpressionFromTokens :: [Token] -> Either ParseError PrimaryExpression
primaryExpressionFromTokens input = parse primaryExpression "PrimaryExpression input" input
