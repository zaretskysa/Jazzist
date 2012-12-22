module LexingTests.NullLiteralTests
(
    nullLiteralTests
) where

import Test.HUnit
import Lexing.NullLiteral
import Lexing.Token
import TestUtils

nullLiteralTests :: Test
nullLiteralTests = TestLabel "Null literal tests" $ TestList 
    [ parseNullLiteral
    , parseCapitalizedNullLiteral
    , parseAnotherCapitalizedNullLiteral
    ]

parseNullLiteral :: Test
parseNullLiteral = TestCase $ assertEqual
    ("Parse " ++ input)
    (Just $ NullLiteralToken)
    (parseWholeTestInput nullLiteral input)
    where input = "null"

parseCapitalizedNullLiteral :: Test
parseCapitalizedNullLiteral = TestCase $ assertEqual
    ("Parse " ++ input)
    (Nothing)
    (parseWholeTestInput nullLiteral input)
    where input = "NULL"

parseAnotherCapitalizedNullLiteral :: Test
parseAnotherCapitalizedNullLiteral = TestCase $ assertEqual
    ("Parse " ++ input)
    (Nothing)
    (parseWholeTestInput nullLiteral input)
    where input = "Null"

