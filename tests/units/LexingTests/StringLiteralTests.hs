module LexingTests.StringLiteralTests
(
    stringLiteralTests
) where

import Test.HUnit
import Lexing.StringLiteral (stringLiteral)
import Lexing.Tokens
import TestUtils

stringLiteralTests :: Test
stringLiteralTests = TestLabel "String literal tests" $ TestList 
    [ emptyDoubleQuotedString
    , helloDoubleQuotedString
    , emptySingleQuotedString
    , helloSingleQuotedString
    , forbiddenDoubleQuoteInDoubleQuotedString
    , forbiddenBackSlashInDoubleQuotedString
    , forbiddenLineTerminatorInDoubleQuotedString
    , forbiddenSingleQuoteInSingleQuotedString
    , forbiddenBackSlashInSingleQuotedString
    , forbiddenLineTerminatorInSingleQuotedString
    , bEscapeSequences
    , fEscapeSequences
    , nEscapeSequences
    , rEscapeSequences
    , tEscapeSequences
    , vEscapeSequences
    , singleQuoteEscapeSequences
    , doubleQuoteEscapeSequences
    , backSlashEscapeSequences
    , characterEscapeSequences
    , nonEscapeCharactersEscapeSequences
    , hexEscapeSequence
    , justHexPrefixEscapeSequence
    , oneDigitHexEscapeSequence
    , threeDigitHexEscapeSequence
    , capitalHexPrefixEscapeSequence
    , unicodeEscapeSequence
    , justUnicodePrefixEscapeSequence
    , oneDigitUnicodeEscapeSequence
    , capitalPrefixUnicodeEscapeSequence
    , fiveDigitsHexEscapeSequence
    , lineContinuation
    , doubleLineContinuation
    , lineContinuationAndNewLine
    , phraseWithLineContinuations
    ]

successful :: String -> String -> Test
successful input result = TestCase $ assertEqual
    ("Parse " ++ input)
    (Just $ StringLiteralToken result)
    (parseWholeTestInput stringLiteral input)

failed :: String -> Test
failed input = TestCase $ assertEqual
    ("Parse " ++ input)
    Nothing
    (parseWholeTestInput stringLiteral input)

emptyDoubleQuotedString :: Test
emptyDoubleQuotedString = successful "\"\"" ""

helloDoubleQuotedString :: Test
helloDoubleQuotedString = successful "\"hello\"" "hello"

emptySingleQuotedString :: Test
emptySingleQuotedString = successful "\'\'" ""

helloSingleQuotedString :: Test
helloSingleQuotedString = successful "\'hello\'" "hello"

forbiddenDoubleQuoteInDoubleQuotedString :: Test
forbiddenDoubleQuoteInDoubleQuotedString = failed "\"\"\""

forbiddenBackSlashInDoubleQuotedString :: Test
forbiddenBackSlashInDoubleQuotedString = failed "\"\\\""

forbiddenLineTerminatorInDoubleQuotedString :: Test
forbiddenLineTerminatorInDoubleQuotedString = failed "\"\n\""

forbiddenSingleQuoteInSingleQuotedString :: Test
forbiddenSingleQuoteInSingleQuotedString = failed "\'\'\'"

forbiddenBackSlashInSingleQuotedString :: Test
forbiddenBackSlashInSingleQuotedString = failed "\'\\\'"

forbiddenLineTerminatorInSingleQuotedString :: Test
forbiddenLineTerminatorInSingleQuotedString = failed "\'\n\'"

bEscapeSequences :: Test
bEscapeSequences = successful ("'\\b'") ("\b")

fEscapeSequences :: Test
fEscapeSequences = successful ("'\\f'") ("\f")

nEscapeSequences :: Test
nEscapeSequences = successful ("'\\n'") ("\n")

rEscapeSequences :: Test
rEscapeSequences = successful ("'\\r'") ("\r")

tEscapeSequences :: Test
tEscapeSequences = successful ("'\\t'") ("\t")

vEscapeSequences :: Test
vEscapeSequences = successful ("'\\v'") ("\v")

singleQuoteEscapeSequences :: Test
singleQuoteEscapeSequences = successful ("'\\''") ("'")

doubleQuoteEscapeSequences :: Test
doubleQuoteEscapeSequences = successful ("'\"'") ("\"")

backSlashEscapeSequences :: Test
backSlashEscapeSequences = successful ("'\\\\'") ("\\")

characterEscapeSequences :: Test
characterEscapeSequences = 
    successful ("' \\b \\f \\n \\r \\t \\v \\\\ \\\" \\' '") (" \b \f \n \r \t \v \\ \" \' ")

nonEscapeCharactersEscapeSequences :: Test
nonEscapeCharactersEscapeSequences = successful 
    ("'\\B\\it\\e\\ \\m\\y\\ \\s\\h\\in\\y\\ \\m\\et\\a\\l\\ \\a\\s\\s\\!'") 
    ("Bite my shiny metal ass!")

hexEscapeSequence :: Test
hexEscapeSequence = successful ("'\\x41\\x42'") ("AB")

justHexPrefixEscapeSequence :: Test
justHexPrefixEscapeSequence = failed "'\\x'"

oneDigitHexEscapeSequence :: Test
oneDigitHexEscapeSequence = failed "'\\x7'"

threeDigitHexEscapeSequence :: Test
threeDigitHexEscapeSequence = successful ("'\\x417'") ("A7")

capitalHexPrefixEscapeSequence :: Test
capitalHexPrefixEscapeSequence = successful ("'\\X41'") ("X41")

unicodeEscapeSequence :: Test
unicodeEscapeSequence = successful ("'\\u0041\\u0042'") ("AB")

justUnicodePrefixEscapeSequence :: Test
justUnicodePrefixEscapeSequence = failed "'\\u'"

oneDigitUnicodeEscapeSequence :: Test
oneDigitUnicodeEscapeSequence = failed "'\\u7'"

capitalPrefixUnicodeEscapeSequence :: Test
capitalPrefixUnicodeEscapeSequence = successful "'\\U0065'" "U0065"

fiveDigitsHexEscapeSequence :: Test
fiveDigitsHexEscapeSequence = successful ("'\\u00410'") ("A0")

lineContinuation :: Test
lineContinuation = successful ("'\\\n'") ("")

doubleLineContinuation :: Test
doubleLineContinuation = successful ("'\\\n\\\n'") ("")

lineContinuationAndNewLine :: Test
lineContinuationAndNewLine = successful ("'\\\n\\n'") ("\n")

phraseWithLineContinuations :: Test
phraseWithLineContinuations = successful 
    ("'\\\nKill \\\nall human\\\ns\\\n,\\\n \\\nmust\\\n \\\nkill all hum\\\nans...\\\n'") 
    ("Kill all humans, must kill all humans...")

