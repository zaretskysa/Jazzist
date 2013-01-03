{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.StatementTests.IfStatementTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils
import ParsingTests.StatementTests.StatementTestUtils


test_ifThenStmt = successful "if (isFoo) foo();"
    (IfStmt 
      (makeExpression "isFoo")
      (ExpressionStmt $ makeExpression "foo()")
      Nothing)

test_nestedIfThenStmt = successful "if (isFoo) if (isBar) fooBar();"
    (IfStmt
      (makeExpression "isFoo")
      (IfStmt
        (makeExpression "isBar")
        (ExpressionStmt $ makeExpression "fooBar()")
        Nothing) 
      Nothing)

test_ifThenElseStmt = successful "if (val) f1(); else f2();"
    (IfStmt 
      (makeExpression "val")
      (ExpressionStmt $ makeExpression "f1()")
      (Just $ ExpressionStmt $ makeExpression "f2()") )

test_nestedIfThenElseStmt = 
    successful "if (val > 0) positive(); else if (val < 0) negative(); else zero();"
    (IfStmt 
      (makeExpression "val > 0")
      (ExpressionStmt $ makeExpression "positive()")
      (Just $ IfStmt
        (makeExpression "val < 0")
        (ExpressionStmt $ makeExpression "negative()")
        (Just $ ExpressionStmt $ makeExpression "zero()") ) )
