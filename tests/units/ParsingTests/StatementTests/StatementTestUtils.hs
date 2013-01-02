{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.StatementTests.StatementTestUtils
(
    successful,
    failed
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser


successful :: String -> Statement -> Assertion
successful input expected = 
    case statementFromString input of
        (Left _) -> assertFailure "Expected successful parsing, but error happened"
        (Right actual) ->  assertEqual expected actual

failed :: String -> Assertion
failed input = 
    case statementFromString input of
        (Left _) -> assertBool True
        (Right result) ->  assertFailure ("Expected failed parsing, but got\n" ++ ppShow result)
