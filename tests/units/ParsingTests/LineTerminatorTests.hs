{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.LineTerminatorTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils

successful :: String -> Program -> Assertion
successful input expected = 
    case programFromString input of
        (Left _) -> assertFailure "Expected successful parsing, but error happened"
        (Right actual) ->  assertEqual expected actual

failed :: String -> Assertion
failed input = 
    case programFromString input of
        (Left _) -> assertBool True
        (Right result) ->  assertFailure ("Expected failed parsing, but got\n" ++ ppShow result)


test_emptyProgram = successful ""
    (Program [])

test_justLineTerminator = successful "\n"
    (Program [])

test_twoLineTerminators = successful "\n\n"
    (Program [])

test_emptyStatementWithLeadingLineTerminator = successful "\n;"
    (Program $ [StatementSourceElement $ EmptyStmt])

test_emptyStatementWithEndingLineTerminator = successful ";\n"
    (Program $ [StatementSourceElement $ EmptyStmt])

test_emptyStatementWithinLineTerminators = successful "\n ; \n"
    (Program $ [StatementSourceElement $ EmptyStmt])

test_twoEmptyStatements = successful "; \n ;"
    (Program 
      [ StatementSourceElement $ EmptyStmt
      , StatementSourceElement $ EmptyStmt
      ] )

test_forWithLineTerminators = 
    successful "\n \nfor \n (\ni\n in\n\n array\n)\n {\n}\n ;"
    (makeProgram " for (i in array) {} ;")

test_arithmeticExpressionWithLineTerminators = 
    successful "\na\n+\nb\n*c\n /\n \nfoo\n(\n)\n & \n4 \n>>> \n2\n;\n"
    (makeProgram "a+b*c / foo() & 4 >>> 2;")
