{-# OPTIONS_GHC -F -pgmF htfpp #-}

module LexingTests.NumericLiteralTests
(
	htf_thisModulesTests
) where

import Control.Applicative ((<$>))
import Test.Framework

import Lexing.NumericLiteral
import Lexing.Token
import TestUtils

successful input result = assertEqual
    (Just $ NumericLiteralToken result)
    (tokenFromLocated <$> parseWholeTestInput numericLiteral input)

failed input = assertEqual
    Nothing
    (parseWholeTestInput numericLiteral input)

test_zeroWithEndingDot = successful "0." 0

test_nineWithEndingDot = successful "9." 9

test_singleDot = failed "."

test_twoLeadingZeros = failed "00"

test_justFractionalPart = successful ".0" 0

test_fractionalPartWithThreeDigits = successful ".123" 0.123

test_intPartWithFractional = successful "7.5" 7.5

-- no signed numbers here, signed numbers are implemented as unary plus/minus expressions
test_intWithLeadingPlus = failed "+7"

test_intWithLeadingMinus = failed"-7"

test_numberWithEndingExpIndicator = failed "1.2e"

test_numberWithExpPart = successful "1.2345e3" 1234.5

test_numberWithPositiveExpPart = successful "1.2345e+3" 1234.5

test_numberWithNegativeExpPart = successful "1.2345e-3" 0.0012345

test_numberWithIntAndExpPartWithoutFrac = successful "1.e3" 1000

test_numberWithFracAndExpPartWithoutInt = successful ".12e3" 120

test_fracNumberWithJustExpPart = failed ".e3"

test_simpleIntNumber = successful "123" 123

test_intNumberWithPositiveExp = successful "123e2" 12300

test_intNumberWithNegativeExp = successful "123e-2" 1.23

test_hexNumber = successful "0x123abc" 0x123abc

test_hexNumberWithCapitals = successful "0X123ABC" 0x123abc

test_hexNumberWithTwoLeadingZeros = failed "00x123abc"

test_justHexPrefix = failed "0x"

test_numberWithTwoHexPrefixes = failed "0x0x123"
