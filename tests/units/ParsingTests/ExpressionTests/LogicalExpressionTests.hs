{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.ExpressionTests.LogicalExpressionTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils


successful :: String -> LogicalOrExpression -> Assertion
successful input expected = 
    case logicalOrExpressionFromString input of
        (Left _) -> assertFailure "Expected successful parsing, but error happened"
        (Right actual) ->  assertEqual expected actual

failed :: String -> Assertion
failed input = 
    case logicalOrExpressionFromString input of
        (Left _) -> assertBool True
        (Right result) ->  assertFailure ("Expected failed parsing, but got\n" ++ ppShow result)


-- LogicalOr expressions

test_unaryLogOrExpr = successful "i"
    (UnaryLogicalOrExpression
      (UnaryLogicalAndExpression
        (makeBitwiseOrExpression "i") ) )

test_binaryLogOrExpr = successful "i || j"
    (BinaryLogicalOrExpression
      (UnaryLogicalOrExpression
        (UnaryLogicalAndExpression
          (makeBitwiseOrExpression "i") ) )
      (UnaryLogicalAndExpression
          (makeBitwiseOrExpression "j") ) )

test_doubleBinaryLogOrExpr = successful "i || j || k"
    (BinaryLogicalOrExpression
      (BinaryLogicalOrExpression
        (UnaryLogicalOrExpression
          (UnaryLogicalAndExpression
            (makeBitwiseOrExpression "i") ) )
        (UnaryLogicalAndExpression
          (makeBitwiseOrExpression "j") ) )
      (UnaryLogicalAndExpression
        (makeBitwiseOrExpression "k") ) )


-- LogicalAnd expressions

test_unaryLogAnd = successful "i"
    (UnaryLogicalOrExpression
      (UnaryLogicalAndExpression
        (makeBitwiseOrExpression "i") ) )

test_binaryLogAnd = successful "i && j"
    (UnaryLogicalOrExpression
      (BinaryLogicalAndExpression
        (UnaryLogicalAndExpression
          (makeBitwiseOrExpression "i") )
        (makeBitwiseOrExpression "j") ) )

test_doubleBinaryLogAnd = successful "i && j && k"
    (UnaryLogicalOrExpression
      (BinaryLogicalAndExpression
        (BinaryLogicalAndExpression
          (UnaryLogicalAndExpression
            (makeBitwiseOrExpression "i") )
          (makeBitwiseOrExpression "j") )
        (makeBitwiseOrExpression "k") ) )

--TODO: add failing tests

--TODO: add NoIn tests

--TODO: add or/and combinations
