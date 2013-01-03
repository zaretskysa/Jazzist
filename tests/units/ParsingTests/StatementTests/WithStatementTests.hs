{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.StatementTests.WithStatementTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils
import ParsingTests.StatementTests.StatementTestUtils


test_with1 = successful "with(i);"
    (WithStmt
      (makeExpression "i")
      EmptyStmt)

test_with2 = successful "with(i) {}"
    (WithStmt
      (makeExpression "i")
      (BlockStmt $ Block []) )

--TODO: no lie terminator tests
--TODO: failing tests
