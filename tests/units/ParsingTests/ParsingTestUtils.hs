module ParsingTests.ParsingTestUtils
(
    makePrimaryExpression,
    makeFunctionExpression,
    makeExpression,
    makeAssignmentExpression,
    makeLeftHandSideExpression,
    makePostfixExpression
) where

import Text.ParserCombinators.Parsec (ParseError)

import Parsing.Parser

makeAstStructure :: String -> (String -> Either ParseError a) -> a
makeAstStructure input parser = case parser input of
    (Left _) -> error ("Can not make ast structure because of parsing error")
    (Right result) -> result

makeMemberExpression :: String -> MemberExpression
makeMemberExpression input = makeAstStructure input memberExpressionFromString

makePrimaryExpression :: String -> PrimaryExpression
makePrimaryExpression input = makeAstStructure input primaryExpressionFromString

makeFunctionExpression :: String -> FunctionExpression
makeFunctionExpression input = makeAstStructure input functionExpressionFromString

makeExpression :: String -> Expression
makeExpression input = makeAstStructure input expressionFromString

makeAssignmentExpression :: String -> AssignmentExpression
makeAssignmentExpression input = makeAstStructure input assignmentExpressionFromString

makeLeftHandSideExpression :: String -> LeftHandSideExpression
makeLeftHandSideExpression input = makeAstStructure input leftHandSideExpressionFromString

makePostfixExpression :: String -> PostfixExpression
makePostfixExpression input = makeAstStructure input postfixExpressionFromString


