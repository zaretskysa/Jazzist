module Lexing.Lexer
(
    module Lexing.LocatedToken,

    tokenize
) where

import Text.ParserCombinators.Parsec hiding (tokens, token)

import Lexing.Token
import Lexing.LocatedToken
import Lexing.NullLiteral
import Lexing.BooleanLiteral
import Lexing.Keyword
import Lexing.StringLiteral
import Lexing.NumericLiteral
import Lexing.Comment
import Lexing.Identifier
import Lexing.Punctuator
import Lexing.LineTerminator
import Lexing.WhiteSpace

tokenize :: String -> Either ParseError [LocatedToken]
tokenize input = parse tokens "JsTokenizer" input

tokens :: Parser [LocatedToken]
tokens = do 
    whiteSpaces
    toks <- sepBy token whiteSpaces
    whiteSpaces >> eof
    return toks

token :: Parser LocatedToken
token = 
    identifier
    <|> try nullLiteral
    <|> try booleanLiteral
    <|> keyword
    <|> stringLiteral
    <|> numericLiteral
    <|> try comment
    <|> punctuator
    <|> lineTerminator
