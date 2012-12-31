{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.ExpressionTests.EqualityExpressionTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils


successful :: String -> EqualityExpression -> Assertion
successful input expected = 
    case equalityExpressionFromString input of
        (Left _) -> assertFailure "Expected successful parsing, but error happened"
        (Right actual) ->  assertEqual expected actual

failed :: String -> Assertion
failed input = 
    case equalityExpressionFromString input of
        (Left _) -> assertBool True
        (Right result) ->  assertFailure ("Expected failed parsing, but got\n" ++ ppShow result)


-- Equality expressions

test_unaryEqExpr = successful "i"
    (RelationalEqualityExpression $ makeRelationalExpression "i")

test_equalsEqExpr = successful "i == j"
    (EqualsEqualityExpression
      (RelationalEqualityExpression
        (makeRelationalExpression "i") )
      (makeRelationalExpression "j") )

test_equalsTwiceEqExpr = successful "i == j == k"
    (EqualsEqualityExpression
      (EqualsEqualityExpression
        (RelationalEqualityExpression
          (makeRelationalExpression "i") )
        (makeRelationalExpression "j") )
      (makeRelationalExpression "k") )

test_notEqualsEqExpr = successful "i != j"
    (NotEqualsEqualityExpression
      (RelationalEqualityExpression
        (makeRelationalExpression "i") )
      (makeRelationalExpression "j") )

test_notEqualsTwiceEqExpr = successful "i != j != k"
    (NotEqualsEqualityExpression
      (NotEqualsEqualityExpression
        (RelationalEqualityExpression
          (makeRelationalExpression "i") )
        (makeRelationalExpression "j") )
      (makeRelationalExpression "k") )

test_strictEqualsEqExpr = successful "i === j"
    (StrictEqualsEqualityExpression
      (RelationalEqualityExpression
        (makeRelationalExpression "i") )
      (makeRelationalExpression "j") )

test_strictEqualsTwiceEqExpr = successful "i === j === k"
    (StrictEqualsEqualityExpression
      (StrictEqualsEqualityExpression
        (RelationalEqualityExpression
          (makeRelationalExpression "i") )
        (makeRelationalExpression "j") )
      (makeRelationalExpression "k") )

test_strictNotEqualsEqExpr = successful "i !== j"
    (StrictNotEqualsEqualityExpression
      (RelationalEqualityExpression
        (makeRelationalExpression "i") )
      (makeRelationalExpression "j") )

test_strictNotEqualsTwiceEqExpr = successful "i !== j !== k"
    (StrictNotEqualsEqualityExpression
      (StrictNotEqualsEqualityExpression
        (RelationalEqualityExpression
          (makeRelationalExpression "i") )
        (makeRelationalExpression "j") )
      (makeRelationalExpression "k") )


--TODO: combine various cases

--TODO: failing tests

--TODO: implement various NoIn tests
