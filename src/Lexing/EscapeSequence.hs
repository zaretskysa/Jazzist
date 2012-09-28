module Lexing.EscapeSequence where

import Data.Char
import Text.ParserCombinators.Parsec

import Lexing.Utils

unicodeEscapeSequenceElement :: Parser Char
unicodeEscapeSequenceElement = char '\\' >> unicodeEscapeSequence

unicodeEscapeSequence :: Parser Char
unicodeEscapeSequence = do
	hs <- char 'u' >> count 4 hexDigit
	return $ chr $ intFromHex hs