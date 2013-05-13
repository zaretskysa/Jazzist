{-# OPTIONS_GHC -F -pgmF htfpp #-}

module LexingTests.PunctuatorTests
(
	htf_thisModulesTests
) where

import Test.Framework

import Lexing.Punctuator
import Lexing.Token
import TestUtils

successful input result = assertEqual
    (Just $ makeLocatedToken $ PunctuatorToken result)
    (parseWholeTestInput punctuator input)

failed input = assertEqual
    Nothing
    (parseWholeTestInput punctuator input)

test_parseLeftCurlyBracket = successful "{" LeftCurlyBracketPunctuator

test_parseRightCurlyBracket = successful "}" RightCurlyBracketPunctuator

test_parseLeftRoundBracket = successful "(" LeftRoundBracketPunctuator

test_parseRightRoundBracket = successful ")" RightRoundBracketPunctuator

test_parseLeftRSquareBracket = successful "[" LeftSquareBracketPunctuator

test_parseRightSquareBracket = successful "]" RightSquareBracketPunctuator

test_parseDot = successful "." DotPunctuator

test_parseSemicolon = successful ";" SemicolonPunctuator

test_parseComma = successful "," CommaPunctuator

test_parseLessThanEquals = successful "<=" LessThanEqualsPunctuator

test_parseGreaterThanEquals = successful ">=" GreaterThanEqualsPunctuator

test_parseLessThan = successful "<" LessThanPunctuator

test_parseGreaterThan = successful ">" GreaterThanPunctuator

test_parseEquals = successful "==" EqualsPunctuator

test_parseNotEquals = successful "!=" NotEqualsPunctuator

test_parseStrictEquals = successful "===" StrictEqualsPunctuator

test_parseStrictNotEquals = successful "!==" StrictNotEqualsPunctuator

test_parseIncrementPlus = successful "++" IncrementPlusPunctuator

test_parsePlus = successful "+" PlusPunctuator

test_parseMinus = successful "-" MinusPunctuator

test_parseMul = successful "*" MulPunctuator

test_parseModulus = successful "%" ModulusPunctuator

test_parseIncrementMinus = successful "--" IncrementMinusPunctuator

test_parseMinusAssign = successful "-=" MinusAssignPunctuator

test_parseLeftShift = successful "<<" LeftShiftPunctuator

test_parseRightShift = successful ">>" RightShiftPunctuator

test_parseUnsignedRightShiftAssign = successful ">>>=" UnsignedRightShiftAssignPunctuator

test_parseUnsignedRightShift = successful ">>>" UnsignedRightShiftPunctuator

test_parseBitwiseAnd = successful "&" BitwiseAndPunctuator

test_parseBitwiseOr = successful "|" BitwiseOrPunctuator

test_parseBitwiseXor = successful "^" BitwiseXorPunctuator

test_parseLogicalNot = successful "!" LogicalNotPunctuator

test_parseBitwiseNot = successful "~" BitwiseNotPunctuator

test_parseLogicalAnd = successful "&&" LogicalAndPunctuator

test_parseLogicalOr = successful "||" LogicalOrPunctuator

test_parseQuestionMark = successful "?" QuestionMarkPunctuator

test_parseColon = successful ":" ColonPunctuator

test_parseAssign = successful "=" AssignPunctuator

test_parsePlusAssign = successful "+=" PlusAssignPunctuator

test_parseMulAssign = successful "*=" MulAssignPunctuator

test_parseModulusAssign = successful "%=" ModulusAssignPunctuator

test_parseLeftShiftAssign = successful "<<=" LeftShiftAssignPunctuator

test_parseRightShiftAssign = successful ">>=" RightShiftAssignPunctuator

test_parseBitwiseAndAssign = successful "&=" BitwiseAndAssignPunctuator

test_parseBitwiseOrAssign = successful "|=" BitwiseOrAssignPunctuator

test_parseBitwiseXorAssign = successful "^=" BitwiseXorAssignPunctuator

test_parseDivOp = successful "/" DivPunctuator

test_parseDivAssign = successful "/=" DivAssignPunctuator
