{-# OPTIONS_GHC -F -pgmF htfpp #-}

module LexingTests.NullLiteralTests
(
    htf_thisModulesTests
) where

import Test.Framework

import Lexing.NullLiteral
import Lexing.Token
import TestUtils

successful input = assertEqual
    (Just NullLiteralToken)
    (parseWholeTestInput nullLiteral input)

failed input = assertEqual
    Nothing
    (parseWholeTestInput nullLiteral input)

test_parseNullLiteral = successful "null"

test_parseCapitalizedNullLiteral = failed "NULL"

test_parseAnotherCapitalizedNullLiteral = failed "Null"
