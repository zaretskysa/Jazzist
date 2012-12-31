{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.ExpressionTests.PostfixExpressionTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils


successful :: String -> PostfixExpression -> Assertion
successful input expected = 
    case postfixExpressionFromString input of
        (Left _) -> assertFailure "Expected successful parsing, but error happened"
        (Right actual) ->  assertEqual expected actual

failed :: String -> Assertion
failed input = 
    case leftHandSideExpressionFromString input of
        (Left _) -> assertBool True
        (Right result) ->  assertFailure ("Expected failed parsing, but got\n" ++ ppShow result)


-- Postfix expressions

test_lhsPostfixExpr = successful "i"
    (LHSPostfixExpression $ makeLeftHandSideExpression "i")

test_incrementPlusPostfixExpr = successful "i++"
    (IncrementPlusPostfixExpression $ makeLeftHandSideExpression "i")

test_incrementMinusPostfixExpr = successful "i--"
    (IncrementMinusPostfixExpression $ makeLeftHandSideExpression "i")
