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

successful :: String -> Punctuator -> Test
successful input result = TestCase $ assertEqual
    ("Parse " ++ input)
    (Just $ PunctuatorToken result)
    (parseWholeTestInput punctuator input)

failed :: String -> Test
failed input = TestCase $ assertEqual
    ("Parse " ++ input)
    Nothing
    (parseWholeTestInput punctuator input)

parseLeftCurlyBracket :: Test
parseLeftCurlyBracket = successful "{" LeftCurlyBracketPunctuator

parseRightCurlyBracket :: Test
parseRightCurlyBracket = successful "}" RightCurlyBracketPunctuator

parseLeftRoundBracket :: Test
parseLeftRoundBracket = successful "(" LeftRoundBracketPunctuator

parseRightRoundBracket :: Test
parseRightRoundBracket = successful ")" RightRoundBracketPunctuator

parseLeftRSquareBracket :: Test
parseLeftRSquareBracket = successful "[" LeftSquareBracketPunctuator

parseRightSquareBracket :: Test
parseRightSquareBracket = successful "]" RightSquareBracketPunctuator

parseDot :: Test
parseDot = successful "." DotPunctuator

parseSemicolon :: Test
parseSemicolon = successful ";" SemicolonPunctuator

parseComma :: Test
parseComma = successful "," CommaPunctuator

parseLessThanEquals :: Test
parseLessThanEquals = successful "<=" LessThanEqualsPunctuator

parseGreaterThanEquals :: Test
parseGreaterThanEquals = successful ">=" GreaterThanEqualsPunctuator

parseLessThan :: Test
parseLessThan = successful "<" LessThanPunctuator

parseGreaterThan :: Test
parseGreaterThan = successful ">" GreaterThanPunctuator

parseEquals :: Test
parseEquals = successful "==" EqualsPunctuator

parseNotEquals :: Test
parseNotEquals = successful "!=" NotEqualsPunctuator

parseStrictEquals :: Test
parseStrictEquals = successful "===" StrictEqualsPunctuator

parseStrictNotEquals :: Test
parseStrictNotEquals = successful "!==" StrictNotEqualsPunctuator

parseIncrementPlus :: Test
parseIncrementPlus = successful "++" IncrementPlusPunctuator

parsePlus :: Test
parsePlus = successful "+" PlusPunctuator

parseMinus :: Test
parseMinus = successful "-" MinusPunctuator

parseMul :: Test
parseMul = successful "*" MulPunctuator

parseModulus :: Test
parseModulus = successful "%" ModulusPunctuator

parseIncrementMinus :: Test
parseIncrementMinus = successful "--" IncrementMinusPunctuator

parseMinusAssign :: Test
parseMinusAssign = successful "-=" MinusAssignPunctuator

parseLeftShift :: Test
parseLeftShift = successful "<<" LeftShiftPunctuator

parseRightShift :: Test
parseRightShift = successful ">>" RightShiftPunctuator

parseUnsignedRightShiftAssign :: Test
parseUnsignedRightShiftAssign = successful ">>>=" UnsignedRightShiftAssignPunctuator

parseUnsignedRightShift :: Test
parseUnsignedRightShift = successful ">>>" UnsignedRightShiftPunctuator

parseBitwiseAnd :: Test
parseBitwiseAnd = successful "&" BitwiseAndPunctuator

parseBitwiseOr :: Test
parseBitwiseOr = successful "|" BitwiseOrPunctuator

parseBitwiseXor :: Test
parseBitwiseXor = successful "^" BitwiseXorPunctuator

parseLogicalNot :: Test
parseLogicalNot = successful "!" LogicalNotPunctuator

parseBitwiseNot :: Test
parseBitwiseNot = successful "~" BitwiseNotPunctuator

parseLogicalAnd :: Test
parseLogicalAnd = successful "&&" LogicalAndPunctuator

parseLogicalOr :: Test
parseLogicalOr = successful "||" LogicalOrPunctuator

parseQuestionMark :: Test
parseQuestionMark = successful "?" QuestionMarkPunctuator

parseColon :: Test
parseColon = successful ":" ColonPunctuator

parseAssign :: Test
parseAssign = successful "=" AssignPunctuator

parsePlusAssign :: Test
parsePlusAssign = successful "+=" PlusAssignPunctuator

parseMulAssign :: Test
parseMulAssign = successful "*=" MulAssignPunctuator

parseModulusAssign :: Test
parseModulusAssign = successful "%=" ModulusAssignPunctuator

parseLeftShiftAssign :: Test
parseLeftShiftAssign = successful "<<=" LeftShiftAssignPunctuator

parseRightShiftAssign :: Test
parseRightShiftAssign = successful ">>=" RightShiftAssignPunctuator

parseBitwiseAndAssign :: Test
parseBitwiseAndAssign = successful "&=" BitwiseAndAssignPunctuator

parseBitwiseOrAssign :: Test
parseBitwiseOrAssign = successful "|=" BitwiseOrAssignPunctuator

parseBitwiseXorAssign :: Test
parseBitwiseXorAssign = successful "^=" BitwiseXorAssignPunctuator

parseDivOp :: Test
parseDivOp = successful "/" DivPunctuator

parseDivAssign :: Test
parseDivAssign = successful "/=" DivAssignPunctuator
