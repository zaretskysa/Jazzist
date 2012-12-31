{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.ExpressionTests.ShiftExpressionTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils


successful :: String -> ShiftExpression -> Assertion
successful input expected = 
    case shiftExpressionFromString input of
        (Left _) -> assertFailure "Expected successful parsing, but error happened"
        (Right actual) ->  assertEqual expected actual

failed :: String -> Assertion
failed input = 
    case shiftExpressionFromString input of
        (Left _) -> assertBool True
        (Right result) ->  assertFailure ("Expected failed parsing, but got\n" ++ ppShow result)


-- Shift expressions

test_additiveShiftExpr = successful "i"
    (AdditiveShiftExpression $ makeAdditiveExpression "i")

test_leftShiftExpr = successful "i << j"
    (LeftShiftExpression
      (AdditiveShiftExpression
        (makeAdditiveExpression "i") )
      (makeAdditiveExpression "j") )

test_doubleLeftShiftExpr = successful "i << j << k"
    (LeftShiftExpression
      (LeftShiftExpression
        (AdditiveShiftExpression
          (makeAdditiveExpression "i") )
        (makeAdditiveExpression "j") )
      (makeAdditiveExpression "k") )

test_rightShiftExpr = successful "i >> j"
    (RightShiftExpression
      (AdditiveShiftExpression
        (makeAdditiveExpression "i") )
      (makeAdditiveExpression "j") )

test_doubleRightShiftExpr = successful "i >> j >> k"
    (RightShiftExpression
      (RightShiftExpression
        (AdditiveShiftExpression
          (makeAdditiveExpression "i") )
        (makeAdditiveExpression "j") )
      (makeAdditiveExpression "k") )

test_unsignedRightShiftExpr = successful "i >>> j"
    (UnsignedRightShiftExpression
      (AdditiveShiftExpression
        (makeAdditiveExpression "i") )
      (makeAdditiveExpression "j") )

test_doubleUnsignedRightShiftExpr = successful "i >>> j >>> k"
    (UnsignedRightShiftExpression
      (UnsignedRightShiftExpression
        (AdditiveShiftExpression
          (makeAdditiveExpression "i") )
        (makeAdditiveExpression "j") )
      (makeAdditiveExpression "k") )


--TODO: combine left and right shifts

--TODO: failing tests
