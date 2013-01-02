{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.StatementTests.BlockStatementTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.StatementTests.StatementTestUtils


test_emptyBlock = successful "{}" 
    (BlockStmt $ Block []) 

test_nestedBlock = successful "{{}}"
    (BlockStmt 
      (Block 
        [BlockStmt 
          (Block []) 
        ] ) )

test_emptyStatementWithinBlock = successful "{;}"
    (BlockStmt
      (Block 
        [EmptyStmt] ) )

test_twoEmptyStatementsWithinBlock = successful "{;;}"
    (BlockStmt
      (Block 
        [ EmptyStmt
        , EmptyStmt
        ] ) )

