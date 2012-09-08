module Lexing.EscapeSequence where

import Data.Char
import Text.ParserCombinators.Parsec

import Lexing.Utils

unicodeEscapeSequenceElement :: Parser Char
unicodeEscapeSequenceElement = char '\\' >> unicodeEscapeSequence

unicodeEscapeSequence :: Parser Char
unicodeEscapeSequence = do
	char 'u'
	hs <- count 4 hexDigit
	return $ chr $ intFromHex hs