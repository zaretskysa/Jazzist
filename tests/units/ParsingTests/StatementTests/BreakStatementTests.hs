{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.StatementTests.BreakStatementTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils
import ParsingTests.StatementTests.StatementTestUtils


test_justBreak = successful "break ;"
    (BreakStmt Nothing)

test_breakWithLabel = successful "break start ;"
    (BreakStmt $ Just $ "start")


--TODO: no lie terminator tests
