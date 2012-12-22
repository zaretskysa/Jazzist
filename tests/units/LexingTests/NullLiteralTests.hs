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

successful :: String -> Test
successful input = TestCase $ assertEqual
    ("Parse " ++ input)
    (Just NullLiteralToken)
    (parseWholeTestInput nullLiteral input)

failed :: String -> Test
failed input = TestCase $ assertEqual
    ("Parse " ++ input)
    Nothing
    (parseWholeTestInput nullLiteral input)

parseNullLiteral :: Test
parseNullLiteral = successful "null"

parseCapitalizedNullLiteral :: Test
parseCapitalizedNullLiteral = failed "NULL"

parseAnotherCapitalizedNullLiteral :: Test
parseAnotherCapitalizedNullLiteral = failed "Null"
