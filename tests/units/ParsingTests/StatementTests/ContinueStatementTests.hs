{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.StatementTests.ContinueStatementTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils
import ParsingTests.StatementTests.StatementTestUtils


test_justContinue = successful "continue ;"
    (ContinueStmt Nothing)

test_continueWithLabel = successful "continue start ;"
    (ContinueStmt $ Just $ "start")

test_justContinueWithLineTerminator = successful "continue \n ;"
    (ContinueStmt Nothing)

test_lineTerminatorWithinLabelledContinue1 = successful "continue start \n ;"
    (ContinueStmt $ Just $ "start")

test_lineTerminatorWithinLabelledContinue2 = failed "continue \n start ;"
