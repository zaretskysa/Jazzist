module LexingTests.BooleanLiteralTests
(
	booleanLiteralTests
) where

import Test.HUnit
import Lexing.BooleanLiteral
import Lexing.Token
import TestUtils

booleanLiteralTests :: Test
booleanLiteralTests = TestLabel "BooleanLiteralTests" $ TestList 
    [ trueValue
    , falseValue
    , falseValueWithEndingNonSpace
    , trueValueWithEndingNonSpace
    , falseValueWithEndingSpace
    , trueValueWithEndingTab
    , falseStringWithBeginningSpace
    , trueStringWithBeginningTab ]

successful :: String -> Bool -> Test
successful input result = TestCase $ assertEqual
    ("Parse " ++ input)
    (Just $ BooleanLiteralToken result)
    (parseWholeTestInput booleanLiteral input)

failed :: String -> Test
failed input = TestCase $ assertEqual
    ("Parse " ++ input)
    Nothing
    (parseWholeTestInput booleanLiteral input)

trueValue :: Test
trueValue = successful "true" True

falseValue :: Test
falseValue = successful "false" False

falseValueWithEndingNonSpace :: Test
falseValueWithEndingNonSpace = failed "falsek"

trueValueWithEndingNonSpace :: Test
trueValueWithEndingNonSpace = failed "true7"

falseValueWithEndingSpace :: Test
falseValueWithEndingSpace = failed "false "

trueValueWithEndingTab :: Test
trueValueWithEndingTab = failed "true\t"

falseStringWithBeginningSpace :: Test
falseStringWithBeginningSpace = failed " false"

trueStringWithBeginningTab :: Test
trueStringWithBeginningTab = failed "\ttrue"
