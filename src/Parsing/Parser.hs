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
    bitwiseOrExpressionFromString,
    logicalOrExpressionFromString,
    conditionalExpressionFromString,
    statementFromString
) where

import Text.ParserCombinators.Parsec (ParseError, runParser, eof)

import Parsing.Ast
import Lexing.LocatedToken
import Lexing.Lexer (tokenize)
import Parsing.TokenParser (TokenParser, initialState)
import qualified Parsing.ProgramParser as PP


parseFromTokens :: [LocatedToken] -> TokenParser a -> Either ParseError a
parseFromTokens tokens parser = 
    runParser (parseWholeInput parser) initialState "" tokens

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

logicalOrExpressionFromString :: String -> Either ParseError LogicalOrExpression
logicalOrExpressionFromString input = parseFromString input PP.logicalOrExpression

conditionalExpressionFromString :: String -> Either ParseError ConditionalExpression
conditionalExpressionFromString input = parseFromString input PP.conditionalExpression

statementFromString :: String -> Either ParseError Statement
statementFromString input = parseFromString input PP.statement
