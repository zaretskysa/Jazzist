{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.StatementTests.ThrowStatementTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils
import ParsingTests.StatementTests.StatementTestUtils


test_throw = successful "throw 666 ;"
    (ThrowStmt $ makeExpression "666")

test_emptyThrow = failed "throw ;"

test_throwWithLineTerminator1 = successful "throw 666 \n ;"
    (ThrowStmt $ makeExpression "666")

test_throwWithLineTerminator2 = failed "throw \n 666 ;"
