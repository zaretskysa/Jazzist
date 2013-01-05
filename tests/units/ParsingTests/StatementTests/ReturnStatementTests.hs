{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.StatementTests.ReturnStatementTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils
import ParsingTests.StatementTests.StatementTestUtils


test_justReturn = successful "return ;"
    (ReturnStmt Nothing)

test_returnWithLabel = successful "return xxx ;"
    (ReturnStmt $ Just $ makeExpression "xxx")

test_justReturnWithLineTerminator = successful "return \n ;"
    (ReturnStmt Nothing)

test_lineTerminatorWithinLabelledReturn1 = successful "return xxx \n ;"
    (ReturnStmt $ Just $ makeExpression "xxx")

test_lineTerminatorWithinLabelledReturn2 = failed "return \n xxx ;"
