module Lexing.BooleanLiteral where

import Text.ParserCombinators.Parsec

import Lexing.Tokens

booleanLiteral :: Parser Token
booleanLiteral = do
	value <- string "true" <|> string "false"
	return $ case value of
		"true" -> BooleanLiteralToken True
		"false" -> BooleanLiteralToken False

isBooleanLiteral :: String -> Bool
isBooleanLiteral s = s `elem` ["true", "false"]
