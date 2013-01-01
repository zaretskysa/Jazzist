module ParsingTests.ParsingTestUtils
(
    makePrimaryExpression,
    makeFunctionExpression,
    makeExpression,
    makeAssignmentExpression,
    makeLeftHandSideExpression,
    makePostfixExpression,
    makeUnaryExpression,
    makeMultiplicativeExpression,
    makeAdditiveExpression,
    makeShiftExpression,
    makeRelationalExpression,
    makeEqualityExpression,
    makeBitwiseOrExpression,
    makeLogicalOrExpression,
    makeConditionalExpression
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

makeUnaryExpression :: String -> UnaryExpression
makeUnaryExpression input = makeAstStructure input unaryExpressionFromString

makeMultiplicativeExpression :: String -> MultiplicativeExpression
makeMultiplicativeExpression input = makeAstStructure input multiplicativeExpressionFromString

makeAdditiveExpression :: String -> AdditiveExpression
makeAdditiveExpression input = makeAstStructure input additiveExpressionFromString

makeShiftExpression :: String -> ShiftExpression
makeShiftExpression input = makeAstStructure input shiftExpressionFromString

makeRelationalExpression :: String -> RelationalExpression
makeRelationalExpression input = makeAstStructure input relationalExpressionFromString

makeEqualityExpression :: String -> EqualityExpression
makeEqualityExpression input = makeAstStructure input equalityExpressionFromString

makeBitwiseOrExpression :: String -> BitwiseOrExpression
makeBitwiseOrExpression input = makeAstStructure input bitwiseOrExpressionFromString

makeLogicalOrExpression :: String -> LogicalOrExpression
makeLogicalOrExpression input = makeAstStructure input logicalOrExpressionFromString

makeConditionalExpression :: String -> ConditionalExpression
makeConditionalExpression input = makeAstStructure input conditionalExpressionFromString
