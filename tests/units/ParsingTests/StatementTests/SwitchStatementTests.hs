{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.StatementTests.SwitchStatementTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils
import ParsingTests.StatementTests.StatementTestUtils


test_switchWithEmptyCaseBlock = successful "switch(i) {}"
    (SwitchStmt
      (makeExpression "i")
      (CaseBlock [] Nothing []) )

test_switchWothoutCaseBlock = failed "switch(i) ;"

test_switchWithEmptyDefault = successful "switch(i) { default: }"
    (SwitchStmt
      (makeExpression "i")
      (CaseBlock
        [] 
        (Just $ DefaultClause [])
        []) )

test_switchWithNonEmptyDefault = successful "switch(i) { default: j++;}"
    (SwitchStmt
      (makeExpression "i")
      (CaseBlock
        [] 
        (Just $ DefaultClause [ExpressionStmt $ makeExpression "j++"])
        []) )

test_switchWithEmptyCase = successful "switch(i) { case 1: }"
    (SwitchStmt
      (makeExpression "i")
      (CaseBlock
        [CaseClause (makeExpression "1") [] ]
        Nothing
        []) )

test_switchWithNonEmptyCase = successful "switch(i) { case 1: j = 1; }"
    (SwitchStmt
      (makeExpression "i")
      (CaseBlock
        [CaseClause 
          (makeExpression "1") 
          [ExpressionStmt $ makeExpression "j = 1"] ]
        Nothing
        []) )

test_switchWithEmptyBeginigCaseAndDeafaultAndEndingCase = 
    successful "switch(i) { case 1: default: case 2: }"
        (SwitchStmt
          (makeExpression "i")
          (CaseBlock
            [CaseClause (makeExpression "1") [] ]
            (Just $ DefaultClause [] )
            [CaseClause (makeExpression "2") [] ] ) )

test_switchWithNonEmptyBeginigCaseAndDeafaultAndEndingCase = 
    successful "switch(i) { case 1: foo(); default: bar(); case 2: buzz(); }"
        (SwitchStmt
          (makeExpression "i")
          (CaseBlock
            [CaseClause (makeExpression "1") [ExpressionStmt $ makeExpression "foo()"] ]
            (Just $ DefaultClause [ExpressionStmt $ makeExpression "bar()"] )
            [CaseClause (makeExpression "2") [ExpressionStmt $ makeExpression "buzz()"] ] ) )


--TODO: failing tests
