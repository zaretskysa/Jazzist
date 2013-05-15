{-# OPTIONS_GHC -F -pgmF htfpp #-}

module LexingTests.LineTerminatorTests
(
    htf_thisModulesTests
) where

import Control.Applicative ((<$>))
import Test.Framework

import Lexing.LineTerminator
import Lexing.Token
import TestUtils

successful input = assertEqual
    (Just $ LineTerminatorToken)
    (tokenFromLocated <$> parseWholeTestInput lineTerminator input)

failed input = assertEqual
    Nothing
    (parseWholeTestInput lineTerminator input)

test_lineFeedString = successful "\x000a"

test_carriageReturnString = successful "\x000d"

test_lineSeparatorString = successful "\x2028"

test_paragraphSeparatorString = successful "\x2029"
