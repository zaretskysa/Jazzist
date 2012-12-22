module LexingTests.LineTerminatorTests
(
    lineTerminatorTests
) where

import Test.HUnit

import Lexing.LineTerminator
import Lexing.Token
import TestUtils

lineTerminatorTests :: Test
lineTerminatorTests = TestLabel "Line terminator tests" $ TestList 
    [ lineFeedString
    , carriageReturnString
    , lineSeparatorString
    , paragraphSeparatorString
    ]

successful :: String -> Test
successful input = TestCase $ assertEqual
    ("Parse " ++ input)
    (Just LineTerminatorToken)
    (parseWholeTestInput lineTerminator input)

failed :: String -> Test
failed input = TestCase $ assertEqual
    ("Parse " ++ input)
    Nothing
    (parseWholeTestInput lineTerminator input)

lineFeedString :: Test
lineFeedString = successful "\x000a"

carriageReturnString :: Test
carriageReturnString = successful "\x000d"

lineSeparatorString :: Test
lineSeparatorString = successful "\x2028"

paragraphSeparatorString :: Test
paragraphSeparatorString = successful "\x2029"
