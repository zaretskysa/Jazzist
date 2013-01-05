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

test_justBreakWithLineTerminator = successful "break \n ;"
    (BreakStmt Nothing)

test_lineTerminatorWithinLabelledBreak1 = failed "break \n start ;"

test_lineTerminatorWithinLabelledBreak2 = successful "break start \n ;"
    (BreakStmt $ Just $ "start")
