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


--TODO: no lie terminator tests
