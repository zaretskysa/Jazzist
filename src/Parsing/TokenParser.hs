module Parsing.TokenParser 
(
    module Text.ParserCombinators.Parsec.Prim,
    module Text.ParserCombinators.Parsec.Combinator,
    module Lexing.Tokens,
    TokenParser,
    acceptToken
) where

import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Pos

import Lexing.Tokens

type TokenParser a = GenParser Token () a

acceptToken :: Token -> TokenParser Token
acceptToken x = token showTok posFromTok testTok
    where
        showTok t = show t
        posFromTok t = initialPos "js tokens source"
        testTok t = if (x == t) then Just t else Nothing