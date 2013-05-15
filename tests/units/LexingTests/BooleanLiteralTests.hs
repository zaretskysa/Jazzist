{-# OPTIONS_GHC -F -pgmF htfpp #-}

module LexingTests.BooleanLiteralTests
(
	htf_thisModulesTests
) where

import Control.Applicative ((<$>))
import Test.Framework

import Lexing.BooleanLiteral
import Lexing.LocatedToken
import TestUtils

successful input result = assertEqual
    (Just $ BooleanLiteralToken result)
    (tokenFromLocated <$> parseWholeTestInput booleanLiteral input)

failed input = assertEqual
    Nothing
    (parseWholeTestInput booleanLiteral input)

test_trueValue = successful "true" True

test_falseValue = successful "false" False

test_falseValueWithEndingNonSpace = failed "falsek"

test_trueValueWithEndingNonSpace = failed "true7"

test_falseValueWithEndingSpace = failed "false "

test_trueValueWithEndingTab = failed "true\t"

test_falseStringWithBeginningSpace = failed " false"

test_trueStringWithBeginningTab = failed "\ttrue"
