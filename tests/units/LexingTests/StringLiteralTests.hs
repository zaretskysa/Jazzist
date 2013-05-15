{-# OPTIONS_GHC -F -pgmF htfpp #-}

module LexingTests.StringLiteralTests
(
    htf_thisModulesTests
) where

import Control.Applicative ((<$>))
import Test.Framework

import Lexing.StringLiteral (stringLiteral)
import Lexing.LocatedToken
import TestUtils

successful input result = assertEqual
    (Just $ StringLiteralToken result)
    (tokenFromLocated <$> parseWholeTestInput stringLiteral input)

failed input = assertEqual
    Nothing
    (parseWholeTestInput stringLiteral input)

test_emptyDoubleQuotedString = successful "\"\"" ""

test_helloDoubleQuotedString = successful "\"hello\"" "hello"

test_emptySingleQuotedString = successful "\'\'" ""

test_helloSingleQuotedString = successful "\'hello\'" "hello"

test_forbiddenDoubleQuoteInDoubleQuotedString = failed "\"\"\""

test_forbiddenBackSlashInDoubleQuotedString = failed "\"\\\""

test_forbiddenLineTerminatorInDoubleQuotedString = failed "\"\n\""

test_forbiddenSingleQuoteInSingleQuotedString = failed "\'\'\'"

test_forbiddenBackSlashInSingleQuotedString = failed "\'\\\'"

test_forbiddenLineTerminatorInSingleQuotedString = failed "\'\n\'"

test_bEscapeSequences = successful ("'\\b'") ("\b")

test_fEscapeSequences = successful ("'\\f'") ("\f")

test_nEscapeSequences = successful ("'\\n'") ("\n")

test_rEscapeSequences = successful ("'\\r'") ("\r")

test_tEscapeSequences = successful ("'\\t'") ("\t")

test_vEscapeSequences = successful ("'\\v'") ("\v")

test_singleQuoteEscapeSequences = successful ("'\\''") ("'")

test_doubleQuoteEscapeSequences = successful ("'\"'") ("\"")

test_backSlashEscapeSequences = successful ("'\\\\'") ("\\")

test_characterEscapeSequences = 
    successful ("' \\b \\f \\n \\r \\t \\v \\\\ \\\" \\' '") (" \b \f \n \r \t \v \\ \" \' ")

test_nonEscapeCharactersEscapeSequences = successful 
    ("'\\B\\it\\e\\ \\m\\y\\ \\s\\h\\in\\y\\ \\m\\et\\a\\l\\ \\a\\s\\s\\!'") 
    ("Bite my shiny metal ass!")

test_hexEscapeSequence = successful ("'\\x41\\x42'") ("AB")

test_justHexPrefixEscapeSequence = failed "'\\x'"

test_oneDigitHexEscapeSequence = failed "'\\x7'"

test_threeDigitHexEscapeSequence = successful ("'\\x417'") ("A7")

test_capitalHexPrefixEscapeSequence = successful ("'\\X41'") ("X41")

test_unicodeEscapeSequence = successful ("'\\u0041\\u0042'") ("AB")

test_justUnicodePrefixEscapeSequence = failed "'\\u'"

test_oneDigitUnicodeEscapeSequence = failed "'\\u7'"

test_capitalPrefixUnicodeEscapeSequence = successful "'\\U0065'" "U0065"

test_fiveDigitsHexEscapeSequence = successful ("'\\u00410'") ("A0")

test_lineContinuation = successful ("'\\\n'") ("")

test_doubleLineContinuation = successful ("'\\\n\\\n'") ("")

test_lineContinuationAndNewLine = successful ("'\\\n\\n'") ("\n")

test_phraseWithLineContinuations = successful 
    ("'\\\nKill \\\nall human\\\ns\\\n,\\\n \\\nmust\\\n \\\nkill all hum\\\nans...\\\n'") 
    ("Kill all humans, must kill all humans...")

test_justNullEscapeSequence = successful ("'\\0'") ("\0")

test_doubleNullEscapeSequence = successful ("'\\0\\0'") ("\0\0")

test_nullEscapeSequenceFollowedByDigit = failed ("'\\07'")

test_nullEscapeSequenceFollowedByLetter = successful ("'\\0a'") ("\0a")
