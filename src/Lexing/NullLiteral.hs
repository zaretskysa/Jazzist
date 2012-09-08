module NullLiteral where

import Text.ParserCombinators.Parsec

import Tokens

nullLiteral :: Parser Token
nullLiteral = do
	string "null"
	return NullLiteralToken

isNullLiteral :: String -> Bool
isNullLiteral s = s == "null"