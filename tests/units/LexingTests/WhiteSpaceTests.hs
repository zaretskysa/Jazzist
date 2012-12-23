{-# OPTIONS_GHC -F -pgmF htfpp #-}

module LexingTests.WhiteSpaceTests
(
    htf_thisModulesTests
) where

import Test.Framework

import Lexing.WhiteSpace
import TestUtils

successful input = assertEqual
    (Just ())
    (parseWholeTestInput whiteSpaces input)

failed input = assertEqual
    Nothing
    (parseWholeTestInput whiteSpaces input)

test_emptyString = successful ""

test_oneSpace = successful " "

test_oneLineBreak = failed "\n"

test_oneTab = successful "\t"

test_oneLetter = failed "f"

test_allSpaces = successful " \v\f\t"

test_allLineBreaks = failed "\n\r"
