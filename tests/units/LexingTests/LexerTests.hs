{-# OPTIONS_GHC -F -pgmF htfpp #-}

module LexingTests.LexerTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes

import Lexing.Lexer

successful :: String -> [Token] -> Assertion
successful input expected = 
    case tokenize input of
        (Left _) -> assertFailure "Left value returned, but right expected"
        (Right tokens) ->  assertEqual expected tokens

failed :: String -> Assertion
failed input = 
    case tokenize input of
        (Left _) -> assertBool True
        (Right _) ->  assertFailure "Right value returned, but left expected"

test_booleanLiterals = successful 
    "true false"
    [BooleanLiteralToken True, BooleanLiteralToken False]

test_comments = successful 
    "//Hello\n/*Galaxy*/"
    [CommentToken "Hello", LineTerminatorToken, CommentToken "Galaxy"]

test_identifiers = successful
    "i hello get set iff Switch DELETE"
    [ IdentifierToken "i"
    , IdentifierToken "hello"
    , IdentifierToken "get"
    , IdentifierToken "set"
    , IdentifierToken "iff"
    , IdentifierToken "Switch"
    , IdentifierToken "DELETE"
    ]

test_keywords = successful
    "break if case throw delete function in while for"
    [ KeywordToken BreakKeyword
    , KeywordToken IfKeyword
    , KeywordToken CaseKeyword
    , KeywordToken ThrowKeyword
    , KeywordToken DeleteKeyword
    , KeywordToken FunctionKeyword
    , KeywordToken InKeyword
    , KeywordToken WhileKeyword
    , KeywordToken ForKeyword
    ]

test_lineTerminators = successful
    "\x000a\x000d\x2028 \x2029"
    [ LineTerminatorToken
    , LineTerminatorToken
    , LineTerminatorToken
    , LineTerminatorToken
    ]

test_nullLiterals = successful
    "null\tnull"
    [NullLiteralToken, NullLiteralToken]

test_numericLiterals = successful
    "123 .5\t2.e0 3e2 4e+2 5e-2"
    [ NumericLiteralToken 123
    , NumericLiteralToken 0.5
    , NumericLiteralToken 2
    , NumericLiteralToken 300
    , NumericLiteralToken 400
    , NumericLiteralToken 0.05]

test_punctuators = successful
    ">>> ? ++ !== == && *="
    [ PunctuatorToken UnsignedRightShiftPunctuator
    , PunctuatorToken QuestionMarkPunctuator
    , PunctuatorToken IncrementPlusPunctuator
    , PunctuatorToken StrictNotEqualsPunctuator
    , PunctuatorToken EqualsPunctuator
    , PunctuatorToken LogicalAndPunctuator
    , PunctuatorToken MulAssignPunctuator
    ]

test_stringLiterals = successful
    "'Bender Bending Rodríguez'  'switch'\t\t'kin dza\\\n dza'"
    [ StringLiteralToken "Bender Bending Rodríguez"
    , StringLiteralToken "switch"
    , StringLiteralToken "kin dza dza"
    ]

test_emptyString = successful "" []

test_varDeclaration = successful
    "var x = 7;"
    [ KeywordToken VarKeyword
    , IdentifierToken "x"
    , PunctuatorToken AssignPunctuator
    , NumericLiteralToken 7
    , PunctuatorToken SemicolonPunctuator
    ]

test_complexVarDeclaration = successful
    "var chainable = arguments.length && ( defaultExtra ||\ntypeof margin !== 'boolean' );"
    [ KeywordToken VarKeyword
    , IdentifierToken "chainable"
    , PunctuatorToken AssignPunctuator
    , IdentifierToken "arguments"
    , PunctuatorToken DotPunctuator
    , IdentifierToken "length"
    , PunctuatorToken LogicalAndPunctuator
    , PunctuatorToken LeftRoundBracketPunctuator
    , IdentifierToken "defaultExtra"
    , PunctuatorToken LogicalOrPunctuator
    , LineTerminatorToken
    , KeywordToken TypeOfKeyword
    , IdentifierToken "margin"
    , PunctuatorToken StrictNotEqualsPunctuator
    , StringLiteralToken "boolean"
    , PunctuatorToken RightRoundBracketPunctuator
    , PunctuatorToken SemicolonPunctuator
    ]

test_incompleteStringLiteral = failed "'milky way"

test_incomleteNumericLiteral = failed "123e"

test_lineBreakInsideStringLiteral = failed "'Phobos \nand Deimos'"
