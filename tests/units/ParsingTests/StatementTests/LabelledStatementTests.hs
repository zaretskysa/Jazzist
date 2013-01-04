{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.StatementTests.LabelledStatementTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils
import ParsingTests.StatementTests.StatementTestUtils


test_labelledEmptyStatement = successful "label: ;"
    (LabelledStmt "label" EmptyStmt)

test_labelledEmptyBlock = successful "label: {}"
    (LabelledStmt 
      "label" 
      (BlockStmt $ Block [] ) )

test_justLabel = failed "label: "
