module LexingTests.KeywordTests
(
	keywordTests
) where

import Test.HUnit
import Lexing.Keyword
import Lexing.Tokens
import TestUtils

keywordTests :: Test
keywordTests = TestLabel "KeywordTests" $ TestList 
	[ parseBreakKeyword
	, parseCaseKeyword
	, parseCatchKeyword
	, parseContinueKeyword
	, parseDebuggerKeyword
	, parseDefaultKeyword
	, parseDeleteKeyword
	, parseDoKeyword
	, parseElseKeyword
	, parseFinallyKeyword
	, parseForKeyword
	, parseFunctionKeyword
	, parseIfKeyword
	, parseInKeyword
	, parseInstanceOfKeyword
	, parseNewKeyword
	, parseReturnKeyword
	, parseSwitchKeyword
	, parseThisKeyword
	, parseThrowKeyword
	, parseTryKeyword
	, parseTypeOfKeyword
	, parseVarKeyword
	, parseVoidKeyword
	, parseWhileKeyword
	, parseWithKeyword ]

parseBreakKeyword :: Test
parseBreakKeyword = TestCase $ assertEqual
    "Parse break keyword"
    (Just $ KeywordToken BreakKeyword)
    (parseTestInput keyword "break")

parseCaseKeyword :: Test
parseCaseKeyword = TestCase $ assertEqual
    "Parse case keyword"
    (Just $ KeywordToken CaseKeyword)
    (parseTestInput keyword "case")

parseCatchKeyword :: Test
parseCatchKeyword = TestCase $ assertEqual
    "Parse catch keyword"
    (Just $ KeywordToken CatchKeyword)
    (parseTestInput keyword "catch")

parseContinueKeyword :: Test
parseContinueKeyword = TestCase $ assertEqual
    "Parse continue keyword"
    (Just $ KeywordToken ContinueKeyword)
    (parseTestInput keyword "continue")

parseDebuggerKeyword :: Test
parseDebuggerKeyword = TestCase $ assertEqual
    "Parse debugger keyword"
    (Just $ KeywordToken DebuggerKeyword)
    (parseTestInput keyword "debugger")

parseDefaultKeyword :: Test
parseDefaultKeyword = TestCase $ assertEqual
    "Parse default keyword"
    (Just $ KeywordToken DefaultKeyword)
    (parseTestInput keyword "default")

parseDeleteKeyword :: Test
parseDeleteKeyword = TestCase $ assertEqual
    "Parse delete keyword"
    (Just $ KeywordToken DeleteKeyword)
    (parseTestInput keyword "delete")

parseDoKeyword :: Test
parseDoKeyword = TestCase $ assertEqual
    "Parse do keyword"
    (Just $ KeywordToken DoKeyword)
    (parseTestInput keyword "do")

parseElseKeyword :: Test
parseElseKeyword = TestCase $ assertEqual
    "Parse else keyword"
    (Just $ KeywordToken ElseKeyword)
    (parseTestInput keyword "else")

parseFinallyKeyword :: Test
parseFinallyKeyword = TestCase $ assertEqual
    "Parse finally keyword"
    (Just $ KeywordToken FinallyKeyword)
    (parseTestInput keyword "finally")

parseForKeyword :: Test
parseForKeyword = TestCase $ assertEqual
    "Parse for keyword"
    (Just $ KeywordToken ForKeyword)
    (parseTestInput keyword "for")

parseFunctionKeyword :: Test
parseFunctionKeyword = TestCase $ assertEqual
    "Parse function keyword"
    (Just $ KeywordToken FunctionKeyword)
    (parseTestInput keyword "function")

parseIfKeyword :: Test
parseIfKeyword = TestCase $ assertEqual
    "Parse if keyword"
    (Just $ KeywordToken IfKeyword)
    (parseTestInput keyword "if")

parseInKeyword :: Test
parseInKeyword = TestCase $ assertEqual
    "Parse in keyword"
    (Just $ KeywordToken InKeyword)
    (parseTestInput keyword "in")

parseInstanceOfKeyword :: Test
parseInstanceOfKeyword = TestCase $ assertEqual
    "Parse instanceof keyword"
    (Just $ KeywordToken InstanceOfKeyword)
    (parseTestInput keyword "instanceof")

parseNewKeyword :: Test
parseNewKeyword = TestCase $ assertEqual
    "Parse new keyword"
    (Just $ KeywordToken NewKeyword)
    (parseTestInput keyword "new")

parseReturnKeyword :: Test
parseReturnKeyword = TestCase $ assertEqual
    "Parse return keyword"
    (Just $ KeywordToken ReturnKeyword)
    (parseTestInput keyword "return")

parseSwitchKeyword :: Test
parseSwitchKeyword = TestCase $ assertEqual
    "Parse switch keyword"
    (Just $ KeywordToken SwitchKeyword)
    (parseTestInput keyword "switch")

parseThisKeyword :: Test
parseThisKeyword = TestCase $ assertEqual
    "Parse this keyword"
    (Just $ KeywordToken ThisKeyword)
    (parseTestInput keyword "this")

parseThrowKeyword :: Test
parseThrowKeyword = TestCase $ assertEqual
    "Parse throw keyword"
    (Just $ KeywordToken ThrowKeyword)
    (parseTestInput keyword "throw")

parseTryKeyword :: Test
parseTryKeyword = TestCase $ assertEqual
    "Parse try keyword"
    (Just $ KeywordToken TryKeyword)
    (parseTestInput keyword "try")

parseTypeOfKeyword :: Test
parseTypeOfKeyword = TestCase $ assertEqual
    "Parse typeof keyword"
    (Just $ KeywordToken TypeOfKeyword)
    (parseTestInput keyword "typeof")

parseVarKeyword :: Test
parseVarKeyword = TestCase $ assertEqual
    "Parse var keyword"
    (Just $ KeywordToken VarKeyword)
    (parseTestInput keyword "var")

parseVoidKeyword :: Test
parseVoidKeyword = TestCase $ assertEqual
    "Parse void keyword"
    (Just $ KeywordToken VoidKeyword)
    (parseTestInput keyword "void")

parseWhileKeyword :: Test
parseWhileKeyword = TestCase $ assertEqual
    "Parse while keyword"
    (Just $ KeywordToken WhileKeyword)
    (parseTestInput keyword "while")

parseWithKeyword :: Test
parseWithKeyword = TestCase $ assertEqual
    "Parse with keyword"
    (Just $ KeywordToken WithKeyword)
    (parseTestInput keyword "with")

