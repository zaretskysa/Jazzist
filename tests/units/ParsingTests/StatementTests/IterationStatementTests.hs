{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.StatementTests.IterationStatementTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils
import ParsingTests.StatementTests.StatementTestUtils


test_doWhile = successful "do ++i; while(i < 10);"
    (IterationStmt
      (DoWhileIterationStatement
        (ExpressionStmt $ makeExpression "++i")
        (makeExpression "i < 10") ) )

test_while = successful "while(i < 10) ++i;"
    (IterationStmt
      (WhileIterationStatement
        (makeExpression "i < 10")
        (ExpressionStmt $ makeExpression "++i") ) )

--TODO: NoIn
test_tripletFor = successful "for (i = 0; i < 10; i++) ;"
    (IterationStmt $ ExprTripletForIterationStatement
      (Just $ makeExpression "i = 0")
      (Just $ makeExpression "i < 10")
      (Just $ makeExpression "i++")
      EmptyStmt)

--TODO: NoIn
test_tripletForWithoutInitializer = successful "for (; i < 10; i++) ;"
    (IterationStmt $ ExprTripletForIterationStatement
      Nothing
      (Just $ makeExpression "i < 10")
      (Just $ makeExpression "i++")
      EmptyStmt)

--TODO: NoIn
test_tripletForWithoutCondition = successful "for (i = 0; ; i++) ;"
    (IterationStmt $ ExprTripletForIterationStatement
      (Just $ makeExpression "i = 0")
      Nothing
      (Just $ makeExpression "i++")
      EmptyStmt)

--TODO: NoIn
test_tripletForWithoutModifier = successful "for (i = 0; i < 10; ) ;"
    (IterationStmt $ ExprTripletForIterationStatement
      (Just $ makeExpression "i = 0")
      (Just $ makeExpression "i < 10")
      Nothing
      EmptyStmt)

--TODO: NoIn
test_emptyTripletFor = successful "for ( ; ; ) ;"
    (IterationStmt $ ExprTripletForIterationStatement
      Nothing
      Nothing
      Nothing
      EmptyStmt)

test_varAndDoubleExprFor = successful "for (var i = 0; i < 10; ++i) ;"
    (IterationStmt $ VarAndDoubleExprForIterationStatement
      ([VariableDeclaration "i" (Just $ Initializer $ makeAssignmentExpression "0")])
      (Just $ makeExpression "i < 10")
      (Just $ makeExpression "++i")
      EmptyStmt)

test_lhsInFor = successful "for (x in xs) ;"
    (IterationStmt $ LHSExprInExprForIterationStatement
      (makeLeftHandSideExpression "x")
      (makeExpression "xs")
      EmptyStmt)

test_varInFor = successful "for (var x in xs) ;"
    (IterationStmt $ VarInExprIteratioinStatement
      (VariableDeclaration "x" Nothing)
      (makeExpression "xs")
      EmptyStmt)


--TODO: failing tests
