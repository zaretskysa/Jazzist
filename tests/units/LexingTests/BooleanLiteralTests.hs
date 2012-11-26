module LexingTests.BooleanLiteralTests
(
	booleanLiteralTests
) where

import Test.HUnit
import Lexing.BooleanLiteral
import Lexing.Tokens
import TestUtils

booleanLiteralTests :: Test
booleanLiteralTests = TestLabel "BooleanLiteralTests" $ TestList 
	[ parseTrueValue
	, parseFalseValue
	, parseFalseValueWithEndingNonSpace
	, parseTrueValueWithEndingNonSpace
	, parseFalseValueWithEndingSpace
	, parseTrueValueWithEndingTab
	, parseFalseStringWithBeginningSpace
	, parseTrueStringWithBeginningTab ]

parseTrueValue :: Test
parseTrueValue = TestCase $ assertEqual
    "Parse true value"
    (Just $ BooleanLiteralToken True)
    (parseTestInput booleanLiteral "true")

parseFalseValue :: Test
parseFalseValue = TestCase $ assertEqual
    "Parse false value"
    (Just $ BooleanLiteralToken False)
    (parseTestInput booleanLiteral "false")

parseFalseValueWithEndingNonSpace :: Test
parseFalseValueWithEndingNonSpace = TestCase $ assertEqual
    "Parse false value with ending non space"
    (Just $ BooleanLiteralToken False)
    (parseTestInput booleanLiteral "falsek")

parseTrueValueWithEndingNonSpace :: Test
parseTrueValueWithEndingNonSpace = TestCase $ assertEqual
    "Parse true value with ending non space"
    (Just $ BooleanLiteralToken True)
    (parseTestInput booleanLiteral "true7")

parseFalseValueWithEndingSpace :: Test
parseFalseValueWithEndingSpace = TestCase $ assertEqual
    "Parse false value with ending space"
    (Just $ BooleanLiteralToken False)
    (parseTestInput booleanLiteral "false ")

parseTrueValueWithEndingTab :: Test
parseTrueValueWithEndingTab = TestCase $ assertEqual
    "Parse true value with ending tab"
    (Just $ BooleanLiteralToken True)
    (parseTestInput booleanLiteral "true\t")

parseFalseStringWithBeginningSpace :: Test
parseFalseStringWithBeginningSpace = TestCase $ assertEqual
    "Parse false string with beginning space"
    Nothing
    (parseTestInput booleanLiteral " false")

parseTrueStringWithBeginningTab :: Test
parseTrueStringWithBeginningTab = TestCase $ assertEqual
    "Parse true string with beginning tab"
    Nothing
    (parseTestInput booleanLiteral "\ttrue")
