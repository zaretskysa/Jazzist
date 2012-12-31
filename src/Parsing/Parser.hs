module Parsing.Parser
(
    module Parsing.Ast,

    programFromString,

    primaryExpressionFromString,
    leftHandSideExpressionFromString,
    memberExpressionFromString,
    functionExpressionFromString,
    expressionFromString,
    assignmentExpressionFromString,
    postfixExpressionFromString,
    unaryExpressionFromString,
    multiplicativeExpressionFromString,
    additiveExpressionFromString,
    shiftExpressionFromString,
    relationalExpressionFromString,
    equalityExpressionFromString,
    bitwiseOrExpressionFromString
) where

import Text.ParserCombinators.Parsec (ParseError, parse, eof)

import Parsing.Ast
import Lexing.Token
import Lexing.Lexer (tokenize)
import Parsing.TokenParser (TokenParser)
import qualified Parsing.ProgramParser as PP


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

programFromString :: String -> Either ParseError Program
programFromString input = parseFromString input PP.program

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

primaryExpressionFromString :: String -> Either ParseError PrimaryExpression
primaryExpressionFromString input = parseFromString input PP.primaryExpression

postfixExpressionFromString :: String -> Either ParseError PostfixExpression
postfixExpressionFromString input = parseFromString input PP.postfixExpression

unaryExpressionFromString :: String -> Either ParseError UnaryExpression
unaryExpressionFromString input = parseFromString input PP.unaryExpression

multiplicativeExpressionFromString :: String -> Either ParseError MultiplicativeExpression
multiplicativeExpressionFromString input = parseFromString input PP.multiplicativeExpression

additiveExpressionFromString :: String -> Either ParseError AdditiveExpression
additiveExpressionFromString input = parseFromString input PP.additiveExpression

shiftExpressionFromString :: String -> Either ParseError ShiftExpression
shiftExpressionFromString input = parseFromString input PP.shiftExpression

relationalExpressionFromString :: String -> Either ParseError RelationalExpression
relationalExpressionFromString input = parseFromString input PP.relationalExpression

equalityExpressionFromString :: String -> Either ParseError EqualityExpression
equalityExpressionFromString input = parseFromString input PP.equalityExpression

bitwiseOrExpressionFromString :: String -> Either ParseError BitwiseOrExpression
bitwiseOrExpressionFromString input = parseFromString input PP.bitwiseOrExpression
