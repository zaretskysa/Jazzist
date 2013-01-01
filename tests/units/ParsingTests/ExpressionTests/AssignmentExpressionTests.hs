{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.ExpressionTests.AssignmentExpressionTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils


successful :: String -> AssignmentExpression -> Assertion
successful input expected = 
    case assignmentExpressionFromString input of
        (Left _) -> assertFailure "Expected successful parsing, but error happened"
        (Right actual) ->  assertEqual expected actual

failed :: String -> Assertion
failed input = 
    case assignmentExpressionFromString input of
        (Left _) -> assertBool True
        (Right result) ->  assertFailure ("Expected failed parsing, but got\n" ++ ppShow result)


test_unaryAssignExpr = successful "i"
    (ConditionalAssignmentExpression
      (makeConditionalExpression "i") )

test_binaryAssignExpr = successful "i = j"
    (AssignmentOperatorExpression
      (makeLeftHandSideExpression "i")
      (SingleAssignOperator)
      (ConditionalAssignmentExpression
         (makeConditionalExpression "j") ) )

test_nestedBinaryAssignExpr = successful "i = j = k"
    (AssignmentOperatorExpression
      (makeLeftHandSideExpression "i")
      (SingleAssignOperator)
      (AssignmentOperatorExpression
         (makeLeftHandSideExpression "j")
         (SingleAssignOperator)
         (ConditionalAssignmentExpression
           (makeConditionalExpression "k") ) ) )

test_binaryMulAssignExpr = successful "i *= j"
    (AssignmentOperatorExpression
      (makeLeftHandSideExpression "i")
      (MulAssignOperator)
      (ConditionalAssignmentExpression
         (makeConditionalExpression "j") ) )

test_binaryDivAssignExpr = successful "i /= j"
    (AssignmentOperatorExpression
      (makeLeftHandSideExpression "i")
      (DivAssignOperator)
      (ConditionalAssignmentExpression
         (makeConditionalExpression "j") ) )

test_binaryModulusAssignExpr = successful "i %= j"
    (AssignmentOperatorExpression
      (makeLeftHandSideExpression "i")
      (ModulusAssignOperator)
      (ConditionalAssignmentExpression
         (makeConditionalExpression "j") ) )

test_binaryPlusAssignExpr = successful "i += j"
    (AssignmentOperatorExpression
      (makeLeftHandSideExpression "i")
      (PlusAssignOperator)
      (ConditionalAssignmentExpression
         (makeConditionalExpression "j") ) )

test_binaryMinusAssignExpr = successful "i -= j"
    (AssignmentOperatorExpression
      (makeLeftHandSideExpression "i")
      (MinusAssignOperator)
      (ConditionalAssignmentExpression
         (makeConditionalExpression "j") ) )

test_binaryLeftShiftAssignExpr = successful "i <<= j"
    (AssignmentOperatorExpression
      (makeLeftHandSideExpression "i")
      (LeftShiftAssignOperator)
      (ConditionalAssignmentExpression
         (makeConditionalExpression "j") ) )

test_binaryRightShiftAssignExpr = successful "i >>= j"
    (AssignmentOperatorExpression
      (makeLeftHandSideExpression "i")
      (RightShiftAssignOperator)
      (ConditionalAssignmentExpression
         (makeConditionalExpression "j") ) )

test_binaryUnsignedRightShiftAssignExpr = successful "i >>>= j"
    (AssignmentOperatorExpression
      (makeLeftHandSideExpression "i")
      (UnsignedRightShiftAssignOperator)
      (ConditionalAssignmentExpression
         (makeConditionalExpression "j") ) )

test_binaryBitwiseAndAssignExpr = successful "i &= j"
    (AssignmentOperatorExpression
      (makeLeftHandSideExpression "i")
      (BitwiseAndAssignOperator)
      (ConditionalAssignmentExpression
         (makeConditionalExpression "j") ) )

test_binaryBitwiseXorAssignExpr = successful "i ^= j"
    (AssignmentOperatorExpression
      (makeLeftHandSideExpression "i")
      (BitwiseXorAssignOperator)
      (ConditionalAssignmentExpression
         (makeConditionalExpression "j") ) )

test_binaryBitwiseOrAssignExpr = successful "i |= j"
    (AssignmentOperatorExpression
      (makeLeftHandSideExpression "i")
      (BitwiseOrAssignOperator)
      (ConditionalAssignmentExpression
         (makeConditionalExpression "j") ) )


--TODO: add failing tests

--TODO: add NoIn tests

--TODO: add various combinations
