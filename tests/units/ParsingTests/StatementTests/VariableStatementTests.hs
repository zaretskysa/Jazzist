{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.StatementTests.VariableStatementTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils
import ParsingTests.StatementTests.StatementTestUtils


test_singleVarWithoutInitialiser = successful "var i;"
    (VariableStmt
      [VariableDeclaration "i" Nothing] )

test_twoVarsWithoutInitialiser = successful "var i, j;"
    (VariableStmt
      [ VariableDeclaration "i" Nothing
      , VariableDeclaration "j" Nothing] )

test_singleVarWithInitialiser = successful "var i = 7;"
    (VariableStmt
      [VariableDeclaration "i" 
        (Just $ Initializer $ makeAssignmentExpression "7")
      ] )

test_twoVarsWithInitialisers = successful "var i = 7, j = 8;"
    (VariableStmt
      [ VariableDeclaration "i" (Just $ Initializer $ makeAssignmentExpression "7")
      , VariableDeclaration "j" (Just $ Initializer $ makeAssignmentExpression "8")
      ] )

test_varsWithAndWithoutInitialiser = successful "var i, j = 8;"
    (VariableStmt
      [ VariableDeclaration "i" Nothing
      , VariableDeclaration "j" (Just $ Initializer $ makeAssignmentExpression "8")
      ] )
