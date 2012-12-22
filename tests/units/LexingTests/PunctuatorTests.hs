module LexingTests.PunctuatorTests
(
	punctuatorTests
) where

import Test.HUnit
import Lexing.Punctuator
import Lexing.Token
import TestUtils

punctuatorTests :: Test
punctuatorTests = TestLabel "PunctuatorTests" $ TestList 
	[ parseLeftCurlyBracket
	, parseRightCurlyBracket
	, parseLeftRoundBracket
	, parseRightRoundBracket
	, parseLeftRSquareBracket
	, parseRightSquareBracket
	, parseDot
	, parseSemicolon
	, parseComma
	, parseLessThanEquals
	, parseGreaterThanEquals
	, parseLessThan
	, parseGreaterThan
	, parseEquals
	, parseNotEquals
	, parseStrictEquals
	, parseStrictNotEquals
	, parseIncrementPlus
	, parsePlus
	, parseMinus
	, parseMul
	, parseModulus
	, parseIncrementMinus
	, parseMinusAssign
	, parseLeftShift
	, parseRightShift
	, parseUnsignedRightShiftAssign
	, parseUnsignedRightShift
	, parseBitwiseAnd
	, parseBitwiseOr
	, parseBitwiseXor
	, parseLogicalNot
	, parseBitwiseNot
	, parseLogicalAnd
	, parseLogicalOr
	, parseQuestionMark
	, parseColon
	, parseAssign
	, parsePlusAssign
	, parseMulAssign
	, parseModulusAssign
	, parseLeftShiftAssign
	, parseRightShiftAssign
	, parseBitwiseAndAssign
	, parseBitwiseOrAssign
	, parseBitwiseXor
	, parseDivOp
	, parseDivAssign ]

parseLeftCurlyBracket :: Test
parseLeftCurlyBracket = TestCase $ assertEqual
    "Parse left curly bracket"
    (Just $ PunctuatorToken LeftCurlyBracketPunctuator)
    (parseTestInput punctuator "{")

parseRightCurlyBracket :: Test
parseRightCurlyBracket = TestCase $ assertEqual
    "Parse right curly bracket"
    (Just $ PunctuatorToken RightCurlyBracketPunctuator)
    (parseTestInput punctuator "}")

parseLeftRoundBracket :: Test
parseLeftRoundBracket = TestCase $ assertEqual
    "Parse left round bracket"
    (Just $ PunctuatorToken LeftRoundBracketPunctuator)
    (parseTestInput punctuator "(")

parseRightRoundBracket :: Test
parseRightRoundBracket = TestCase $ assertEqual
    "Parse right round bracket"
    (Just $ PunctuatorToken RightRoundBracketPunctuator)
    (parseTestInput punctuator ")")

parseLeftRSquareBracket :: Test
parseLeftRSquareBracket = TestCase $ assertEqual
    "Parse left square bracket"
    (Just $ PunctuatorToken LeftSquareBracketPunctuator)
    (parseTestInput punctuator "[")

parseRightSquareBracket :: Test
parseRightSquareBracket = TestCase $ assertEqual
    "Parse right square bracket"
    (Just $ PunctuatorToken RightSquareBracketPunctuator)
    (parseTestInput punctuator "]")

parseDot :: Test
parseDot = TestCase $ assertEqual
    "Parse dot"
    (Just $ PunctuatorToken DotPunctuator)
    (parseTestInput punctuator ".")

parseSemicolon :: Test
parseSemicolon = TestCase $ assertEqual
    "Parse semicolon"
    (Just $ PunctuatorToken SemicolonPunctuator)
    (parseTestInput punctuator ";")

parseComma :: Test
parseComma = TestCase $ assertEqual
    "Parse comma"
    (Just $ PunctuatorToken CommaPunctuator)
    (parseTestInput punctuator ",")

parseLessThanEquals :: Test
parseLessThanEquals = TestCase $ assertEqual
    "Parse <="
    (Just $ PunctuatorToken LessThanEqualsPunctuator)
    (parseTestInput punctuator "<=")

parseGreaterThanEquals :: Test
parseGreaterThanEquals = TestCase $ assertEqual
    "Parse >="
    (Just $ PunctuatorToken GreaterThanEqualsPunctuator)
    (parseTestInput punctuator ">=")

parseLessThan :: Test
parseLessThan = TestCase $ assertEqual
    "Parse <"
    (Just $ PunctuatorToken LessThanPunctuator)
    (parseTestInput punctuator "<")

parseGreaterThan :: Test
parseGreaterThan = TestCase $ assertEqual
    "Parse >"
    (Just $ PunctuatorToken GreaterThanPunctuator)
    (parseTestInput punctuator ">")

parseEquals :: Test
parseEquals = TestCase $ assertEqual
    "Parse =="
    (Just $ PunctuatorToken EqualsPunctuator)
    (parseTestInput punctuator "==")

parseNotEquals :: Test
parseNotEquals = TestCase $ assertEqual
    "Parse !="
    (Just $ PunctuatorToken NotEqualsPunctuator)
    (parseTestInput punctuator "!=")

parseStrictEquals :: Test
parseStrictEquals = TestCase $ assertEqual
    "Parse ==="
    (Just $ PunctuatorToken StrictEqualsPunctuator)
    (parseTestInput punctuator "===")

parseStrictNotEquals :: Test
parseStrictNotEquals = TestCase $ assertEqual
    "Parse !=="
    (Just $ PunctuatorToken StrictNotEqualsPunctuator)
    (parseTestInput punctuator "!==")

parseIncrementPlus :: Test
parseIncrementPlus = TestCase $ assertEqual
    "Parse ++"
    (Just $ PunctuatorToken IncrementPlusPunctuator)
    (parseTestInput punctuator "++")

parsePlus :: Test
parsePlus = TestCase $ assertEqual
    "Parse +"
    (Just $ PunctuatorToken PlusPunctuator)
    (parseTestInput punctuator "+")

parseMinus :: Test
parseMinus = TestCase $ assertEqual
    "Parse -"
    (Just $ PunctuatorToken MinusPunctuator)
    (parseTestInput punctuator "-")

parseMul :: Test
parseMul = TestCase $ assertEqual
    "Parse *"
    (Just $ PunctuatorToken MulPunctuator)
    (parseTestInput punctuator "*")

parseModulus :: Test
parseModulus = TestCase $ assertEqual
    "Parse %"
    (Just $ PunctuatorToken ModulusPunctuator)
    (parseTestInput punctuator "%")

parseIncrementMinus :: Test
parseIncrementMinus = TestCase $ assertEqual
    "Parse --"
    (Just $ PunctuatorToken IncrementMinusPunctuator)
    (parseTestInput punctuator "--")

parseMinusAssign :: Test
parseMinusAssign = TestCase $ assertEqual
    "Parse -="
    (Just $ PunctuatorToken MinusAssignPunctuator)
    (parseTestInput punctuator "-=")

parseLeftShift :: Test
parseLeftShift = TestCase $ assertEqual
    "Parse <<"
    (Just $ PunctuatorToken LeftShiftPunctuator)
    (parseTestInput punctuator "<<")

parseRightShift :: Test
parseRightShift = TestCase $ assertEqual
    "Parse >>"
    (Just $ PunctuatorToken RightShiftPunctuator)
    (parseTestInput punctuator ">>")

parseUnsignedRightShiftAssign :: Test
parseUnsignedRightShiftAssign = TestCase $ assertEqual
    "Parse >>>="
    (Just $ PunctuatorToken UnsignedRightShiftAssignPunctuator)
    (parseTestInput punctuator ">>>=")

parseUnsignedRightShift :: Test
parseUnsignedRightShift = TestCase $ assertEqual
    "Parse >>>"
    (Just $ PunctuatorToken UnsignedRightShiftPunctuator)
    (parseTestInput punctuator ">>>")

parseBitwiseAnd :: Test
parseBitwiseAnd = TestCase $ assertEqual
    "Parse &"
    (Just $ PunctuatorToken BitwiseAndPunctuator)
    (parseTestInput punctuator "&")

parseBitwiseOr :: Test
parseBitwiseOr = TestCase $ assertEqual
    "Parse |"
    (Just $ PunctuatorToken BitwiseOrPunctuator)
    (parseTestInput punctuator "|")

parseBitwiseXor :: Test
parseBitwiseXor = TestCase $ assertEqual
    "Parse ^"
    (Just $ PunctuatorToken BitwiseXorPunctuator)
    (parseTestInput punctuator "^")

parseLogicalNot :: Test
parseLogicalNot = TestCase $ assertEqual
    "Parse !"
    (Just $ PunctuatorToken LogicalNotPunctuator)
    (parseTestInput punctuator "!")

parseBitwiseNot :: Test
parseBitwiseNot = TestCase $ assertEqual
    "Parse ~"
    (Just $ PunctuatorToken BitwiseNotPunctuator)
    (parseTestInput punctuator "~")

parseLogicalAnd :: Test
parseLogicalAnd = TestCase $ assertEqual
    "Parse &&"
    (Just $ PunctuatorToken LogicalAndPunctuator)
    (parseTestInput punctuator "&&")

parseLogicalOr :: Test
parseLogicalOr = TestCase $ assertEqual
    "Parse ||"
    (Just $ PunctuatorToken LogicalOrPunctuator)
    (parseTestInput punctuator "||")

parseQuestionMark :: Test
parseQuestionMark = TestCase $ assertEqual
    "Parse ?"
    (Just $ PunctuatorToken QuestionMarkPunctuator)
    (parseTestInput punctuator "?")

parseColon :: Test
parseColon = TestCase $ assertEqual
    "Parse :"
    (Just $ PunctuatorToken ColonPunctuator)
    (parseTestInput punctuator ":")

parseAssign :: Test
parseAssign = TestCase $ assertEqual
    "Parse ="
    (Just $ PunctuatorToken AssignPunctuator)
    (parseTestInput punctuator "=")

parsePlusAssign :: Test
parsePlusAssign = TestCase $ assertEqual
    "Parse +="
    (Just $ PunctuatorToken PlusAssignPunctuator)
    (parseTestInput punctuator "+=")

parseMulAssign :: Test
parseMulAssign = TestCase $ assertEqual
    "Parse *="
    (Just $ PunctuatorToken MulAssignPunctuator)
    (parseTestInput punctuator "*=")

parseModulusAssign :: Test
parseModulusAssign = TestCase $ assertEqual
    "Parse %="
    (Just $ PunctuatorToken ModulusAssignPunctuator)
    (parseTestInput punctuator "%=")

parseLeftShiftAssign :: Test
parseLeftShiftAssign = TestCase $ assertEqual
    "Parse <<="
    (Just $ PunctuatorToken LeftShiftAssignPunctuator)
    (parseTestInput punctuator "<<=")

parseRightShiftAssign :: Test
parseRightShiftAssign = TestCase $ assertEqual
    "Parse >>="
    (Just $ PunctuatorToken RightShiftAssignPunctuator)
    (parseTestInput punctuator ">>=")

parseBitwiseAndAssign :: Test
parseBitwiseAndAssign = TestCase $ assertEqual
    "Parse &="
    (Just $ PunctuatorToken BitwiseAndAssignPunctuator)
    (parseTestInput punctuator "&=")

parseBitwiseOrAssign :: Test
parseBitwiseOrAssign = TestCase $ assertEqual
    "Parse |="
    (Just $ PunctuatorToken BitwiseOrAssignPunctuator)
    (parseTestInput punctuator "|=")

parseBitwiseXorAssign :: Test
parseBitwiseXorAssign = TestCase $ assertEqual
    "Parse ^="
    (Just $ PunctuatorToken BitwiseXorAssignPunctuator)
    (parseTestInput punctuator "^=")

parseDivOp :: Test
parseDivOp = TestCase $ assertEqual
    "Parse /"
    (Just $ PunctuatorToken DivPunctuator)
    (parseTestInput punctuator "/")

parseDivAssign :: Test
parseDivAssign = TestCase $ assertEqual
    "Parse /="
    (Just $ PunctuatorToken DivAssignPunctuator)
    (parseTestInput punctuator "/=")
