module Lexing.NullLiteral
(
    module Lexing.Token,

    nullLiteral,
    isNullLiteral
) where

import Text.ParserCombinators.Parsec

import Lexing.Token

nullLiteral :: Parser Token
nullLiteral = string "null" >> return NullLiteralToken

isNullLiteral :: String -> Bool
isNullLiteral s = s == "null"
