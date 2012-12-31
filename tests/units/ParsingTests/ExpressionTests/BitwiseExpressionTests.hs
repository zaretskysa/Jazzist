{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.ExpressionTests.BitwiseExpressionTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils


successful :: String -> BitwiseOrExpression -> Assertion
successful input expected = 
    case bitwiseOrExpressionFromString input of
        (Left _) -> assertFailure "Expected successful parsing, but error happened"
        (Right actual) ->  assertEqual expected actual

failed :: String -> Assertion
failed input = 
    case bitwiseOrExpressionFromString input of
        (Left _) -> assertBool True
        (Right result) ->  assertFailure ("Expected failed parsing, but got\n" ++ ppShow result)


-- BitwiseOr

test_unaryBitOrExpr = successful "i"
    (UnaryBitwiseOrExpression
      (UnaryBitwiseXorExpression
        (UnaryBitwiseAndExpression
          (makeEqualityExpression "i") ) ) )

test_binaryBitOrExpr = successful "i | j"
    (BinaryBitwiseOrExpression
      (UnaryBitwiseOrExpression
        (UnaryBitwiseXorExpression
          (UnaryBitwiseAndExpression
            (makeEqualityExpression "i") ) ) )
      (UnaryBitwiseXorExpression
        (UnaryBitwiseAndExpression
          (makeEqualityExpression "j") ) ) )

test_doubleBinaryBitOrExpr = successful "i | j | k"
    (BinaryBitwiseOrExpression
      (BinaryBitwiseOrExpression
        (UnaryBitwiseOrExpression
          (UnaryBitwiseXorExpression
            (UnaryBitwiseAndExpression
              (makeEqualityExpression "i") ) ) )
        (UnaryBitwiseXorExpression
          (UnaryBitwiseAndExpression
            (makeEqualityExpression "j") ) ) )
      (UnaryBitwiseXorExpression
        (UnaryBitwiseAndExpression
          (makeEqualityExpression "k") ) ) )


-- BitwiseXor expression

test_unaryBitXorExpr = successful "i"
    (UnaryBitwiseOrExpression
      (UnaryBitwiseXorExpression
        (UnaryBitwiseAndExpression
          (makeEqualityExpression "i") ) ) )

test_binaryBitXorExpr = successful "i ^ j"
    (UnaryBitwiseOrExpression
      (BinaryBitwiseXorExpression
        (UnaryBitwiseXorExpression
          (UnaryBitwiseAndExpression
            (makeEqualityExpression "i") ) )
        (UnaryBitwiseAndExpression
          (makeEqualityExpression "j") ) ) )

test_doubleBinaryBitXorExpr = successful "i ^ j ^ k"
    (UnaryBitwiseOrExpression
      (BinaryBitwiseXorExpression
        (BinaryBitwiseXorExpression
          (UnaryBitwiseXorExpression
            (UnaryBitwiseAndExpression
              (makeEqualityExpression "i") ) )
          (UnaryBitwiseAndExpression
            (makeEqualityExpression "j") ) )
        (UnaryBitwiseAndExpression
          (makeEqualityExpression "k") ) ) )


-- BitwiseAnd expressions

test_unaryBitAndExpr = successful "i"
    (UnaryBitwiseOrExpression
      (UnaryBitwiseXorExpression
        (UnaryBitwiseAndExpression
          (makeEqualityExpression "i") ) ) )

test_binaryBitAndExpr = successful "i & j"
    (UnaryBitwiseOrExpression
      (UnaryBitwiseXorExpression
        (BinaryBitwiseAndExpression
          (UnaryBitwiseAndExpression
            (makeEqualityExpression "i") )
          (makeEqualityExpression "j") ) ) )

test_doubleBinaryBitAndExpr = successful "i & j & k"
    (UnaryBitwiseOrExpression
      (UnaryBitwiseXorExpression
        (BinaryBitwiseAndExpression
          (BinaryBitwiseAndExpression
            (UnaryBitwiseAndExpression
              (makeEqualityExpression "i") )
            (makeEqualityExpression "j") )
          (makeEqualityExpression "k") ) ) )

--TODO: combine various cases

--TODO: failing tests

--TODO: implement various NoIn tests
