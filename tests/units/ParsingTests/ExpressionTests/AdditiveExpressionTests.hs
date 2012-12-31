{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.ExpressionTests.AdditiveExpressionTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils


successful :: String -> AdditiveExpression -> Assertion
successful input expected = 
    case additiveExpressionFromString input of
        (Left _) -> assertFailure "Expected successful parsing, but error happened"
        (Right actual) ->  assertEqual expected actual

failed :: String -> Assertion
failed input = 
    case additiveExpressionFromString input of
        (Left _) -> assertBool True
        (Right result) ->  assertFailure ("Expected failed parsing, but got\n" ++ ppShow result)


-- Multiplicative expressions

test_multAdditiveExpr = successful "i"
    (MultAdditiveExpression $ makeMultiplicativeExpression "i")

test_plusAdditiveExpr = successful "i + j"
    (PlusAdditiveExpression
      (MultAdditiveExpression
        (makeMultiplicativeExpression "i") )
      (makeMultiplicativeExpression "j") )

test_plusPlusAdditiveExpr = successful "i + j + k"
    (PlusAdditiveExpression
      (PlusAdditiveExpression
        (MultAdditiveExpression
          (makeMultiplicativeExpression "i") )
        (makeMultiplicativeExpression "j") )
      (makeMultiplicativeExpression "k") )

test_minusAdditiveExpr = successful "i - j"
    (MinusAdditiveExpression
      (MultAdditiveExpression
        (makeMultiplicativeExpression "i") )
      (makeMultiplicativeExpression "j") )

test_minusMinusAdditiveExpr = successful "i - j - k"
    (MinusAdditiveExpression
      (MinusAdditiveExpression
        (MultAdditiveExpression
          (makeMultiplicativeExpression "i") )
        (makeMultiplicativeExpression "j") )
      (makeMultiplicativeExpression "k") )

-- TODO: implement +- and -+ combinations
