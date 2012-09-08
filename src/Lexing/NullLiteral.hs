module Lexing.NullLiteral where

import Text.ParserCombinators.Parsec

import Lexing.Tokens

nullLiteral :: Parser Token
nullLiteral = do
	string "null"
	return NullLiteralToken

isNullLiteral :: String -> Bool
isNullLiteral s = s == "null"