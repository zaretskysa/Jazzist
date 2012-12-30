module Parsing.Parser
(
    module Parsing.Ast,

    parseString,

    primaryExpressionFromString,
    leftHandSideExpressionFromString,
    memberExpressionFromString,
    functionExpressionFromString,
    expressionFromString,
    assignmentExpressionFromString
) where

import Text.ParserCombinators.Parsec (ParseError, parse, eof)

import Parsing.Ast
import Lexing.Token
import Lexing.Lexer (tokenize)
import Parsing.TokenParser (TokenParser)
import qualified Parsing.ProgramParser as PP

--TODO: unify function names

parseString :: String -> Either ParseError Program
parseString input = case tokenize input of
    Left err -> Left err
    Right toks -> parseTokens toks

parseTokens :: [Token] -> Either ParseError Program
parseTokens input = parse PP.program "JsParser" input

primaryExpressionFromString :: String -> Either ParseError PrimaryExpression
primaryExpressionFromString input = case tokenize input of
    Left err -> Left err
    Right toks -> primaryExpressionFromTokens toks

primaryExpressionFromTokens :: [Token] -> Either ParseError PrimaryExpression
primaryExpressionFromTokens input = parse PP.primaryExpression "PrimaryExpression input" input

-- TODO: use this functions instead of copy-paste
parseFromTokens :: [Token] -> TokenParser a -> Either ParseError a
parseFromTokens tokens parser = parse (parseWholeInput parser) "tokens input" tokens

parseWholeInput :: TokenParser a -> TokenParser a
parseWholeInput parser = do
    result <- parser
    eof
    return result

parseFromString :: String -> TokenParser a -> Either ParseError a
parseFromString input parser = case tokenize input of
    Left err -> Left err
    Right toks -> parseFromTokens toks parser

---

memberExpressionFromString :: String -> Either ParseError MemberExpression
memberExpressionFromString input = parseFromString input PP.memberExpression

functionExpressionFromString :: String -> Either ParseError FunctionExpression
functionExpressionFromString input = parseFromString input PP.functionExpression

expressionFromString :: String -> Either ParseError Expression
expressionFromString input = parseFromString input PP.expression

leftHandSideExpressionFromString :: String -> Either ParseError LeftHandSideExpression
leftHandSideExpressionFromString input = parseFromString input PP.leftHandSideExpression

assignmentExpressionFromString :: String -> Either ParseError AssignmentExpression
assignmentExpressionFromString input = parseFromString input PP.assignmentExpression
