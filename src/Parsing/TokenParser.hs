module Parsing.TokenParser 
(
    module Text.ParserCombinators.Parsec.Prim,
    module Text.ParserCombinators.Parsec.Combinator,
    module Lexing.Tokens,
    TokenParser,
    maybeParse,
    identifierToken,
    punctuatorToken,
    keywordToken,
    nullLiteralToken
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

acceptAnyToken :: TokenParser Token
acceptAnyToken = token showTok posFromTok testTok
    where
        showTok t = show t
        posFromTok t = initialPos "js tokens source"
        testTok t = Just t

identifierToken :: TokenParser String
identifierToken = try $ do
    t <- acceptAnyToken 
    case t of
        IdentifierToken id -> return id
        otherwise -> fail "expecting identifier token"

punctuatorToken :: Punctuator -> TokenParser Punctuator
punctuatorToken p = try $ do
    tok <- acceptAnyToken 
    case tok of
        PunctuatorToken x -> if x == p 
            then return p 
            else fail $ "expecting punctuator: " ++ show p
        otherwise -> fail "expecting punctuator token"

keywordToken :: Keyword -> TokenParser Keyword
keywordToken k = try $ do
    tok <- acceptAnyToken 
    case tok of
        KeywordToken x -> if x == k 
            then return k
            else fail $ "expecting keyword: " ++ show k
        otherwise -> fail "expecting keyword token"

nullLiteralToken :: TokenParser ()
nullLiteralToken = try $ do
    tok <- acceptAnyToken 
    case tok of
        NullLiteralToken -> return ()
        otherwise -> fail "expecting null literal token"


maybeParse p = justParse p <|> return Nothing

justParse p = do
    value <- p
    return $ Just value


