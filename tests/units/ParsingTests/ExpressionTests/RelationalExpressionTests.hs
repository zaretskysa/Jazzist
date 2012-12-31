{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.ExpressionTests.RelationalExpressionTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils


successful :: String -> RelationalExpression -> Assertion
successful input expected = 
    case relationalExpressionFromString input of
        (Left _) -> assertFailure "Expected successful parsing, but error happened"
        (Right actual) ->  assertEqual expected actual

failed :: String -> Assertion
failed input = 
    case relationalExpressionFromString input of
        (Left _) -> assertBool True
        (Right result) ->  assertFailure ("Expected failed parsing, but got\n" ++ ppShow result)


-- Relational expressions

test_shiftRelExpr = successful "i"
    (ShiftRelationalExpression $ makeShiftExpression "i")

test_lessThanRelExpr = successful "i < j"
    (LessThanRelationalExpression
      (ShiftRelationalExpression
        (makeShiftExpression "i") )
      (makeShiftExpression "j") )

test_lessThanLessThanRelExpr = successful "i < j < k"
    (LessThanRelationalExpression
      (LessThanRelationalExpression
        (ShiftRelationalExpression
          (makeShiftExpression "i") )
        (makeShiftExpression "j") )
      (makeShiftExpression "k") )

test_greaterThanRelExpr = successful "i > j"
    (GreaterThanRelationalExpression
      (ShiftRelationalExpression
        (makeShiftExpression "i") )
      (makeShiftExpression "j") )

test_greaterThanGreaterThanRelExpr = successful "i > j > k"
    (GreaterThanRelationalExpression
      (GreaterThanRelationalExpression
        (ShiftRelationalExpression
          (makeShiftExpression "i") )
        (makeShiftExpression "j") )
      (makeShiftExpression "k") )

test_lessThanEqualsRelExpr = successful "i <= j"
    (LessThanEqualsRelationalExpression
      (ShiftRelationalExpression
        (makeShiftExpression "i") )
      (makeShiftExpression "j") )

test_lessThanEqualsTwiceRelExpr = successful "i <= j <= k"
    (LessThanEqualsRelationalExpression
      (LessThanEqualsRelationalExpression
        (ShiftRelationalExpression
          (makeShiftExpression "i") )
        (makeShiftExpression "j") )
      (makeShiftExpression "k") )

test_greaterThanEqualsRelExpr = successful "i >= j"
    (GreaterThanEqualsRelationalExpression
      (ShiftRelationalExpression
        (makeShiftExpression "i") )
      (makeShiftExpression "j") )

test_greaterThanEqualsTwiceRelExpr = successful "i >= j >= k"
    (GreaterThanEqualsRelationalExpression
      (GreaterThanEqualsRelationalExpression
        (ShiftRelationalExpression
          (makeShiftExpression "i") )
        (makeShiftExpression "j") )
      (makeShiftExpression "k") )

test_instanceOfRelExpr = successful "foo instanceof Foo"
    (InstanceOfRelationalExpression
      (ShiftRelationalExpression
        (makeShiftExpression "foo") )
      (makeShiftExpression "Foo") )

test_instanceOfTwiceRelExpr = successful "foo instanceof Foo instanceof Bar"
    (InstanceOfRelationalExpression
      (InstanceOfRelationalExpression
        (ShiftRelationalExpression
          (makeShiftExpression "foo") )
        (makeShiftExpression "Foo") )
      (makeShiftExpression "Bar") )

test_inRelExpr = successful "foo in foos"
    (InRelationalExpression
      (ShiftRelationalExpression
        (makeShiftExpression "foo") )
      (makeShiftExpression "foos") )

test_inTwiceRelExpr = successful "foo in foos in bars"
    (InRelationalExpression
      (InRelationalExpression
        (ShiftRelationalExpression
          (makeShiftExpression "foo") )
        (makeShiftExpression "foos") )
      (makeShiftExpression "bars") )


--TODO: combine less and greater

--TODO: failing tests

--TODO: implement various NoIn tests
