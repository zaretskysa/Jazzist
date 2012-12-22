module LexingTests.WhiteSpaceTests
(
    whiteSpaceTests
) where

import Test.HUnit
import Lexing.WhiteSpace
import TestUtils

whiteSpaceTests :: Test
whiteSpaceTests = TestLabel "White space tests" $ TestList 
    [ emptyString
    , oneSpace
    , oneTab
    , oneLineBreak
    , oneLetter
    , allSpaces
    , allLineBreaks
    ]

successful :: String -> Test
successful input = TestCase $ assertEqual
    ("Parse " ++ input)
    (Just ())
    (parseWholeTestInput whiteSpaces input)

failed :: String -> Test
failed input = TestCase $ assertEqual
    ("Parse " ++ input)
    Nothing
    (parseWholeTestInput whiteSpaces input)

emptyString :: Test
emptyString = successful ""

oneSpace :: Test
oneSpace = successful " "

oneLineBreak :: Test
oneLineBreak = failed "\n"

oneTab :: Test
oneTab = successful "\t"

oneLetter :: Test
oneLetter = failed "f"

allSpaces :: Test
allSpaces = successful " \v\f\t"

allLineBreaks :: Test
allLineBreaks = failed "\n\r"
