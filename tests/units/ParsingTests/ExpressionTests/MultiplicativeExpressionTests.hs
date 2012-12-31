{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.ExpressionTests.MultiplicativeExpressionTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils


successful :: String -> MultiplicativeExpression -> Assertion
successful input expected = 
    case multiplicativeExpressionFromString input of
        (Left _) -> assertFailure "Expected successful parsing, but error happened"
        (Right actual) ->  assertEqual expected actual

failed :: String -> Assertion
failed input = 
    case multiplicativeExpressionFromString input of
        (Left _) -> assertBool True
        (Right result) ->  assertFailure ("Expected failed parsing, but got\n" ++ ppShow result)


-- Multiplicative expressions

test_unaryMultExpr = successful "i"
    (UnaryMultiplicativeExpression $ makeUnaryExpression "i")

test_mulMultExpr = successful "i * j"
    (MulMultiplicativeExpression 
      (UnaryMultiplicativeExpression 
        (makeUnaryExpression "i") )
      (makeUnaryExpression "j") )

test_twoMulsMultExpr = successful "i * j * k"
    (MulMultiplicativeExpression 
      (MulMultiplicativeExpression 
        (UnaryMultiplicativeExpression 
          (makeUnaryExpression "i") )
        (makeUnaryExpression "j") )
      (makeUnaryExpression "k") )

test_divMultExpr = successful "i / j"
    (DivMultiplicativeExpression
      (UnaryMultiplicativeExpression 
        (makeUnaryExpression "i") )
      (makeUnaryExpression "j") )

test_twoDivsMultExpr = successful "i / j / k"
    (DivMultiplicativeExpression 
      (DivMultiplicativeExpression 
        (UnaryMultiplicativeExpression 
          (makeUnaryExpression "i") )
        (makeUnaryExpression "j") )
      (makeUnaryExpression "k") )

test_modlusMultExpr = successful "i % j"
    (ModulusMultiplicativeExpression
      (UnaryMultiplicativeExpression 
        (makeUnaryExpression "i") )
      (makeUnaryExpression "j") )

test_doubleModulusMultExpr = successful "i % j % k"
    (ModulusMultiplicativeExpression 
      (ModulusMultiplicativeExpression 
        (UnaryMultiplicativeExpression 
          (makeUnaryExpression "i") )
        (makeUnaryExpression "j") )
      (makeUnaryExpression "k") )

