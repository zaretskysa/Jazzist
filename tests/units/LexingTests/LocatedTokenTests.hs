{-# OPTIONS_GHC -F -pgmF htfpp #-}

module LexingTests.LocatedTokenTests
(
    htf_thisModulesTests
) where


import Test.Framework
import Test.Framework.TestTypes
import Text.Parsec.Pos (newPos)

import Lexing.Lexer
import Lexing.LocatedToken


successful :: String -> [LocatedToken] -> Assertion
successful input expected = 
    case tokenize input of
        (Left _) -> assertFailure "Left value returned, but right expected"
        (Right tokens) ->  assertEqual expected tokens

failed :: String -> Assertion
failed input = 
    case tokenize input of
        (Left _) -> assertBool True
        (Right _) ->  assertFailure "Right value returned, but left expected"

test_booleanLiteralSourcePos = successful 
    "  true"
    [ LocatedToken (BooleanLiteralToken True) (newPos "JsTokenizer" 1 3)]

test_commentSourcePos = successful 
    "  //comment"
    [ LocatedToken (CommentToken "comment") (newPos "JsTokenizer" 1 3)]

test_identifierSourcePos = successful 
    "  fooBar"
    [ LocatedToken (IdentifierToken "fooBar") (newPos "JsTokenizer" 1 3)]

test_keywordSourcePos = successful 
    "  break"
    [ LocatedToken (KeywordToken BreakKeyword) (newPos "JsTokenizer" 1 3)]

test_lineTerminatorSourcePos = successful 
    "  \n\n"
    [ LocatedToken LineTerminatorToken (newPos "JsTokenizer" 1 3)
    , LocatedToken LineTerminatorToken (newPos "JsTokenizer" 2 1)
    ]

test_nulllLiteralSourcePos = successful 
    "  null"
    [ LocatedToken NullLiteralToken (newPos "JsTokenizer" 1 3)]

test_numericLiteralSourcePos = successful 
    "  123"
    [ LocatedToken (NumericLiteralToken 123) (newPos "JsTokenizer" 1 3)]

test_punctuatorSourcePos = successful 
    "  ;"
    [ LocatedToken (PunctuatorToken SemicolonPunctuator) (newPos "JsTokenizer" 1 3)]

test_stringLiteralSourcePos = successful 
    "  \'hello\'"
    [ LocatedToken (StringLiteralToken "hello") (newPos "JsTokenizer" 1 3)]

test_allTokensSourcePos = successful 
    "\n123 hello null \ntrue \'foo\' + break"
    [ LocatedToken (LineTerminatorToken) (newPos "JsTokenizer" 1 1)
    , LocatedToken (NumericLiteralToken 123) (newPos "JsTokenizer" 2 1)
    , LocatedToken (IdentifierToken "hello") (newPos "JsTokenizer" 2 5)
    , LocatedToken (NullLiteralToken) (newPos "JsTokenizer" 2 11)
    , LocatedToken (LineTerminatorToken) (newPos "JsTokenizer" 2 16)
    , LocatedToken (BooleanLiteralToken True) (newPos "JsTokenizer" 3 1)
    , LocatedToken (StringLiteralToken "foo") (newPos "JsTokenizer" 3 6)
    , LocatedToken (PunctuatorToken PlusPunctuator) (newPos "JsTokenizer" 3 12)
    , LocatedToken (KeywordToken BreakKeyword) (newPos "JsTokenizer" 3 14)
    ]
