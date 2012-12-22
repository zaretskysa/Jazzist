module Lexing.WhiteSpace
(
    whiteSpace,
    whiteSpaces
) where

import Text.ParserCombinators.Parsec

import Lexing.LineTerminator

whiteSpace :: Parser Char
whiteSpace = (notFollowedBy lineTerminatorChar) >> space

whiteSpaces :: Parser ()
whiteSpaces = skipMany whiteSpace
