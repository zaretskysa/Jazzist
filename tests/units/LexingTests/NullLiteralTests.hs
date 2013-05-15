{-# OPTIONS_GHC -F -pgmF htfpp #-}

module LexingTests.NullLiteralTests
(
    htf_thisModulesTests
) where

import Control.Applicative ((<$>))
import Test.Framework

import Lexing.NullLiteral
import Lexing.LocatedToken
import TestUtils

successful input = assertEqual
    (Just $ NullLiteralToken)
    (tokenFromLocated <$> parseWholeTestInput nullLiteral input)

failed input = assertEqual
    Nothing
    (parseWholeTestInput nullLiteral input)

test_parseNullLiteral = successful "null"

test_parseCapitalizedNullLiteral = failed "NULL"

test_parseAnotherCapitalizedNullLiteral = failed "Null"
