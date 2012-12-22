module Lexing.NullLiteral where

import Text.ParserCombinators.Parsec

import Lexing.Token

nullLiteral :: Parser Token
nullLiteral = string "null" >> return NullLiteralToken

isNullLiteral :: String -> Bool
isNullLiteral s = s == "null"
