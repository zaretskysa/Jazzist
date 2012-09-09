module Parsing.Parsers.Program where

import Text.ParserCombinators.Parsec

import Parsing.Data.Program
import Parsing.TokenParser
import Parsing.Parsers.SourceElement


program :: TokenParser Program
program = do
    srcElements <- many sourceElement
    return $ Program srcElements
