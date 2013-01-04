{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.StatementTests.TryStatementTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils
import ParsingTests.StatementTests.StatementTestUtils


test_emptyTryAndEmptyCatch = successful "try {} catch(e) {}"
    (TryStmt $ BlockCatchTryStatement
      (Block [])
      (Catch "e" (Block []) ) )

test_nonEmptyTryAndNonEmptyCatch = successful "try { foo(); } catch(e) { bar(); }"
    (TryStmt $ BlockCatchTryStatement
      (Block [ExpressionStmt $ makeExpression "foo()"] )
      (Catch "e" (Block [ExpressionStmt $ makeExpression "bar()"] ) ) )

test_emptyTryAndEmptyFinally = successful "try {} finally {}"
    (TryStmt $ BlockFinallyTryStatement
      (Block [])
      (Finally $ Block []) )

test_nonEmptyTryAndNonEmptyFinally = successful "try { foo(); } finally { bar(); }"
    (TryStmt $ BlockFinallyTryStatement
      (Block [ExpressionStmt $ makeExpression "foo()"])
      (Finally $ Block [ExpressionStmt $ makeExpression "bar()"]) )


test_emptyTryCatchFinally = 
    successful "try {} catch (e) {} finally {}"
        (TryStmt $ BlockCatchFinallyTryStatement
          (Block [])
          (Catch "e" (Block []) )
          (Finally $ Block []) )

test_nonEmptyTryCatchFinally = 
    successful "try { foo(); } catch (e) { bar(); } finally { buz(); }"
        (TryStmt $ BlockCatchFinallyTryStatement
          (Block [ExpressionStmt $ makeExpression "foo()"])
          (Catch "e" (Block [ExpressionStmt $ makeExpression "bar()"]) )
          (Finally $ Block [ExpressionStmt $ makeExpression "buz()"]) )


--TODO: no line terminator tests
