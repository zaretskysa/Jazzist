module Lexing.Lexer
(
    module Lexing.Token,

    tokenize
) where

import Text.ParserCombinators.Parsec hiding (tokens, token)

import Lexing.Token
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

tokenize :: String -> Either ParseError [Token]
tokenize input = parse tokens "JsTokenizer" input

tokens :: Parser [Token]
tokens = spaces >> endBy token whiteSpaces

token :: Parser Token
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
