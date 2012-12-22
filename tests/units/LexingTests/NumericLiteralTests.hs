module LexingTests.NumericLiteralTests
(
	numericLiteralTests
) where

import Test.HUnit
import Lexing.NumericLiteral
import Lexing.Token
import TestUtils

numericLiteralTests :: Test
numericLiteralTests = TestLabel "Numeric literal tests" $ TestList 
    [ parseZeroWithEndingDot
    , parseNineWithEndingDot
    , parseSingleDot
    , parseTwoLeadingZeros
    , parseJustFractionalPart
    , parseFractionalPartWithThreeDigits
    , parseIntPartWithFractional
    , parseIntWithLeadingPlus
    , parseIntWithLeadingMinus
    , parseNumberWithEndingExpIndicator
    , parseNumberWithExpPart
    , parseNumberWithPositiveExpPart
    , parseNumberWithNegativeExpPart
    , parseNumberWithIntAndExpPartWithoutFrac
    , parseNumberWithFracAndExpPartWithoutInt
    , parseFracNumberWithJustExpPart
    , parseSimpleIntNumber
    , parseIntNumberWithPositiveExp
    , parseIntNumberWithNegativeExp
    , parseIntNumberWithZeroExp
    , parseHexNumber
    , parseHexNumberWithCapitals
    , parseHexNumberWithTwoLeadingZeros
    , parseJustHexPrefix
    , parseNumberWithTwoHexPrefixes
    ]

parseZeroWithEndingDot :: Test
parseZeroWithEndingDot = TestCase $ assertEqual
    ("Parse " ++ input)
    (Just $ NumericLiteralToken 0)
    (parseTestInput numericLiteral input)
    where input = "0."

parseNineWithEndingDot :: Test
parseNineWithEndingDot = TestCase $ assertEqual
    "Parse 9."
    (Just $ NumericLiteralToken 9)
    (parseTestInput numericLiteral "9.")

parseSingleDot :: Test
parseSingleDot = TestCase $ assertEqual
    "Parse ."
    (Nothing)
    (parseTestInput numericLiteral ".")

parseTwoLeadingZeros :: Test
parseTwoLeadingZeros = TestCase $ assertEqual
    "Parse 00"
    (Just $ NumericLiteralToken 0)
    (parseTestInput numericLiteral "00")

parseJustFractionalPart :: Test
parseJustFractionalPart = TestCase $ assertEqual
    "Parse .0"
    (Just $ NumericLiteralToken 0)
    (parseTestInput numericLiteral ".0")

parseFractionalPartWithThreeDigits :: Test
parseFractionalPartWithThreeDigits = TestCase $ assertEqual
    ("Parse " ++ input)
    (Just $ NumericLiteralToken 0.123)
    (parseTestInput numericLiteral input)
    where input = ".123"

parseIntPartWithFractional :: Test
parseIntPartWithFractional = TestCase $ assertEqual
    ("Parse " ++ input)
    (Just $ NumericLiteralToken 7.5)
    (parseTestInput numericLiteral input)
    where input = "7.5"

-- no signed numbers here, signed numbers are implemented as unary plus/minus expressions
parseIntWithLeadingPlus :: Test
parseIntWithLeadingPlus = TestCase $ assertEqual
    ("Parse " ++ input)
    (Nothing)
    (parseTestInput numericLiteral input)
    where input = "+7"

parseIntWithLeadingMinus :: Test
parseIntWithLeadingMinus = TestCase $ assertEqual
    ("Parse " ++ input)
    (Nothing)
    (parseTestInput numericLiteral input)
    where input = "-7"

parseNumberWithEndingExpIndicator :: Test
parseNumberWithEndingExpIndicator = TestCase $ assertEqual
    ("Parse " ++ input)
    (Nothing)
    (parseWholeTestInput numericLiteral input)
    where input = "1.2e"

parseNumberWithExpPart :: Test
parseNumberWithExpPart = TestCase $ assertEqual
    ("Parse " ++ input)
    (Just $ NumericLiteralToken 1234.5)
    (parseWholeTestInput numericLiteral input)
    where input = "1.2345e3"

parseNumberWithPositiveExpPart :: Test
parseNumberWithPositiveExpPart = TestCase $ assertEqual
    ("Parse " ++ input)
    (Just $ NumericLiteralToken 1234.5)
    (parseWholeTestInput numericLiteral input)
    where input = "1.2345e+3"

parseNumberWithNegativeExpPart :: Test
parseNumberWithNegativeExpPart = TestCase $ assertEqual
    ("Parse " ++ input)
    (Just $ NumericLiteralToken 0.0012345)
    (parseWholeTestInput numericLiteral input)
    where input = "1.2345e-3"

parseNumberWithIntAndExpPartWithoutFrac :: Test
parseNumberWithIntAndExpPartWithoutFrac = TestCase $ assertEqual
    ("Parse " ++ input)
    (Just $ NumericLiteralToken 1000)
    (parseWholeTestInput numericLiteral input)
    where input = "1.e3"

parseNumberWithFracAndExpPartWithoutInt :: Test
parseNumberWithFracAndExpPartWithoutInt = TestCase $ assertEqual
    ("Parse " ++ input)
    (Just $ NumericLiteralToken 120)
    (parseWholeTestInput numericLiteral input)
    where input = ".12e3"

parseFracNumberWithJustExpPart :: Test
parseFracNumberWithJustExpPart = TestCase $ assertEqual
    ("Parse " ++ input)
    (Nothing)
    (parseWholeTestInput numericLiteral input)
    where input = ".e3"

parseSimpleIntNumber :: Test
parseSimpleIntNumber = TestCase $ assertEqual
    ("Parse " ++ input)
    (Just $ NumericLiteralToken 123)
    (parseWholeTestInput numericLiteral input)
    where input = "123"

parseIntNumberWithPositiveExp :: Test
parseIntNumberWithPositiveExp = TestCase $ assertEqual
    ("Parse " ++ input)
    (Just $ NumericLiteralToken 12300)
    (parseWholeTestInput numericLiteral input)
    where input = "123e2"

parseIntNumberWithNegativeExp :: Test
parseIntNumberWithNegativeExp = TestCase $ assertEqual
    ("Parse " ++ input)
    (Just $ NumericLiteralToken 1.23)
    (parseWholeTestInput numericLiteral input)
    where input = "123e-2"

parseIntNumberWithZeroExp :: Test
parseIntNumberWithZeroExp = TestCase $ assertEqual
    ("Parse " ++ input)
    (Just $ NumericLiteralToken 123)
    (parseWholeTestInput numericLiteral input)
    where input = "123e0"

parseHexNumber :: Test
parseHexNumber = TestCase $ assertEqual
    ("Parse " ++ input)
    (Just $ NumericLiteralToken 0x123abc)
    (parseWholeTestInput numericLiteral input)
    where input = "0x123abc"

parseHexNumberWithCapitals :: Test
parseHexNumberWithCapitals = TestCase $ assertEqual
    ("Parse " ++ input)
    (Just $ NumericLiteralToken 0x123abc)
    (parseWholeTestInput numericLiteral input)
    where input = "0X123ABC"

parseHexNumberWithTwoLeadingZeros :: Test
parseHexNumberWithTwoLeadingZeros = TestCase $ assertEqual
    ("Parse " ++ input)
    (Nothing)
    (parseWholeTestInput numericLiteral input)
    where input = "00x123abc"

parseJustHexPrefix :: Test
parseJustHexPrefix = TestCase $ assertEqual
    ("Parse " ++ input)
    (Nothing)
    (parseWholeTestInput numericLiteral input)
    where input = "0x"

parseNumberWithTwoHexPrefixes :: Test
parseNumberWithTwoHexPrefixes = TestCase $ assertEqual
    ("Parse " ++ input)
    (Nothing)
    (parseWholeTestInput numericLiteral input)
    where input = "0x0x123"
