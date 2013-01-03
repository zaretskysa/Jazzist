{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.StatementTests.ExpressionStatementTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils
import ParsingTests.StatementTests.StatementTestUtils


test_singleVarExpr = successful "i;"
    (ExpressionStmt $ makeExpression "i")

test_onePlusOneExpr = successful "1 + 1;"
    (ExpressionStmt $ makeExpression "1 + 1")
