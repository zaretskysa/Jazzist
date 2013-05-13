module Lexing.NullLiteral
(
    module Lexing.LocatedToken,

    nullLiteral,
    isNullLiteral
) where

import Text.ParserCombinators.Parsec

import Lexing.LocatedToken

nullLiteral :: Parser LocatedToken
nullLiteral = do
    string "null"
    return $ makeLocatedToken NullLiteralToken

isNullLiteral :: String -> Bool
isNullLiteral s = s == "null"
