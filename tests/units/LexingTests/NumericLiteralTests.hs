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
    [ zeroWithEndingDot
    , nineWithEndingDot
    , singleDot
    , twoLeadingZeros
    , justFractionalPart
    , fractionalPartWithThreeDigits
    , intPartWithFractional
    , intWithLeadingPlus
    , intWithLeadingMinus
    , numberWithEndingExpIndicator
    , numberWithExpPart
    , numberWithPositiveExpPart
    , numberWithNegativeExpPart
    , numberWithIntAndExpPartWithoutFrac
    , numberWithFracAndExpPartWithoutInt
    , fracNumberWithJustExpPart
    , simpleIntNumber
    , intNumberWithPositiveExp
    , intNumberWithNegativeExp
    , intNumberWithZeroExp
    , hexNumber
    , hexNumberWithCapitals
    , hexNumberWithTwoLeadingZeros
    , justHexPrefix
    , numberWithTwoHexPrefixes
    ]

successful :: String -> Double -> Test
successful input result = TestCase $ assertEqual
    ("Parse " ++ input)
    (Just $ NumericLiteralToken result)
    (parseWholeTestInput numericLiteral input)

failed :: String -> Test
failed input = TestCase $ assertEqual
    ("Parse " ++ input)
    Nothing
    (parseWholeTestInput numericLiteral input)

zeroWithEndingDot :: Test
zeroWithEndingDot = successful "0." 0

nineWithEndingDot :: Test
nineWithEndingDot = successful "9." 9

singleDot :: Test
singleDot = failed "."

twoLeadingZeros :: Test
twoLeadingZeros = failed "00"

justFractionalPart :: Test
justFractionalPart = successful ".0" 0

fractionalPartWithThreeDigits :: Test
fractionalPartWithThreeDigits = successful ".123" 0.123

intPartWithFractional :: Test
intPartWithFractional = successful "7.5" 7.5

-- no signed numbers here, signed numbers are implemented as unary plus/minus expressions
intWithLeadingPlus :: Test
intWithLeadingPlus = failed "+7"

intWithLeadingMinus :: Test
intWithLeadingMinus = failed"-7"

numberWithEndingExpIndicator :: Test
numberWithEndingExpIndicator = failed "1.2e"

numberWithExpPart :: Test
numberWithExpPart = successful "1.2345e3" 1234.5

numberWithPositiveExpPart :: Test
numberWithPositiveExpPart = successful "1.2345e+3" 1234.5

numberWithNegativeExpPart :: Test
numberWithNegativeExpPart = successful "1.2345e-3" 0.0012345

numberWithIntAndExpPartWithoutFrac :: Test
numberWithIntAndExpPartWithoutFrac = successful "1.e3" 1000

numberWithFracAndExpPartWithoutInt :: Test
numberWithFracAndExpPartWithoutInt = successful ".12e3" 120

fracNumberWithJustExpPart :: Test
fracNumberWithJustExpPart = failed ".e3"

simpleIntNumber :: Test
simpleIntNumber = successful "123" 123

intNumberWithPositiveExp :: Test
intNumberWithPositiveExp = successful "123e2" 12300

intNumberWithNegativeExp :: Test
intNumberWithNegativeExp = successful "123e-2" 1.23

intNumberWithZeroExp :: Test
intNumberWithZeroExp = successful "123e0" 123

hexNumber :: Test
hexNumber = successful "0x123abc" 0x123abc

hexNumberWithCapitals :: Test
hexNumberWithCapitals = successful "0X123ABC" 0x123abc

hexNumberWithTwoLeadingZeros :: Test
hexNumberWithTwoLeadingZeros = failed "00x123abc"

justHexPrefix :: Test
justHexPrefix = failed "0x"

numberWithTwoHexPrefixes :: Test
numberWithTwoHexPrefixes = failed "0x0x123"
