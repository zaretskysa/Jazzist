{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.ExpressionTests.UnaryExpressionTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils


successful :: String -> UnaryExpression -> Assertion
successful input expected = 
    case unaryExpressionFromString input of
        (Left _) -> assertFailure "Expected successful parsing, but error happened"
        (Right actual) ->  assertEqual expected actual

failed :: String -> Assertion
failed input = 
    case unaryExpressionFromString input of
        (Left _) -> assertBool True
        (Right result) ->  assertFailure ("Expected failed parsing, but got\n" ++ ppShow result)


-- Unary expressions

test_postfixUnaryExpr = successful "i"
    (PostfixUnaryExpression $ makePostfixExpression "i")

test_deleteUnaryExpr = successful "delete i"
    (DeleteUnaryExpression
      (PostfixUnaryExpression 
        (makePostfixExpression "i") ) )

test_deleteDeleteUnaryExpr = successful "delete delete i"
    (DeleteUnaryExpression
      (DeleteUnaryExpression
        (PostfixUnaryExpression 
          (makePostfixExpression "i") ) ) )

test_voidUnaryExpr = successful "void i"
    (VoidUnaryExpression
      (PostfixUnaryExpression 
        (makePostfixExpression "i") ) )

test_voidVoidUnaryExpr = successful "void void i"
    (VoidUnaryExpression
      (VoidUnaryExpression
        (PostfixUnaryExpression 
          (makePostfixExpression "i") ) ) )

test_typeOfUnaryExpr = successful "typeof i"
    (TypeOfUnaryExpression
      (PostfixUnaryExpression 
        (makePostfixExpression "i") ) )

test_typeOfTypeOfUnaryExpr = successful "typeof typeof i"
    (TypeOfUnaryExpression
      (TypeOfUnaryExpression
        (PostfixUnaryExpression 
          (makePostfixExpression "i") ) ) )

test_IncrementPlusUnaryExpr = successful "++i"
    (IncrementPlusUnaryExpression
      (PostfixUnaryExpression 
        (makePostfixExpression "i") ) )

test_doubleIncrementPlusUnaryExpr = successful "++ ++i"
    (IncrementPlusUnaryExpression
      (IncrementPlusUnaryExpression
        (PostfixUnaryExpression 
          (makePostfixExpression "i") ) ) )

test_IncrementMinusUnaryExpr = successful "--i"
    (IncrementMinusUnaryExpression
      (PostfixUnaryExpression 
        (makePostfixExpression "i") ) )

test_doubleIncrementMinusUnaryExpr = successful "-- --i"
    (IncrementMinusUnaryExpression
      (IncrementMinusUnaryExpression
        (PostfixUnaryExpression 
          (makePostfixExpression "i") ) ) )

test_plusUnaryExpr = successful "+i"
    (PlusUnaryExpression
      (PostfixUnaryExpression 
        (makePostfixExpression "i") ) )

test_doublePlusUnaryExpr = successful "+ + i"
    (PlusUnaryExpression
      (PlusUnaryExpression
        (PostfixUnaryExpression 
          (makePostfixExpression "i") ) ) )

test_minusUnaryExpr = successful "-i"
    (MinusUnaryExpression
      (PostfixUnaryExpression 
        (makePostfixExpression "i") ) )

test_doubleMinusUnaryExpr = successful "- - i"
    (MinusUnaryExpression
      (MinusUnaryExpression
        (PostfixUnaryExpression 
          (makePostfixExpression "i") ) ) )

test_bitwiseNotUnaryExpr = successful "~i"
    (BitwiseNotUnaryExpression
      (PostfixUnaryExpression 
        (makePostfixExpression "i") ) )

test_doubleBitwiseNotUnaryExpr = successful "~ ~ i"
    (BitwiseNotUnaryExpression
      (BitwiseNotUnaryExpression
        (PostfixUnaryExpression 
          (makePostfixExpression "i") ) ) )

test_LogicalNotUnaryExpr = successful "!i"
    (LogicalNotUnaryExpression
      (PostfixUnaryExpression 
        (makePostfixExpression "i") ) )

test_doubleLogicalNotUnaryExpr = successful "! ! i"
    (LogicalNotUnaryExpression
      (LogicalNotUnaryExpression
        (PostfixUnaryExpression 
          (makePostfixExpression "i") ) ) )
