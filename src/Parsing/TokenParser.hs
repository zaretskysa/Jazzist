module Parsing.TokenParser 
(
    module Text.ParserCombinators.Parsec.Prim,
    module Text.ParserCombinators.Parsec.Combinator,
    module Lexing.LocatedToken,

    TokenParser,
    
    identifierToken,
    punctuatorToken,
    keywordToken,
    nullLiteralToken,
    booleanLiteralToken,
    numericLiteralToken,
    stringLiteralToken,
    identifierName,
    lineTerminatorToken,

    ParserState(..),
    initialState,

    semicolon,
    autoSemicolon,

) where

import Control.Monad

import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim

import Lexing.LocatedToken


type TokenParser a = GenParser LocatedToken ParserState a

data ParserState = ParserState 
    { previousTokenIsLineTerminator :: Bool }
    deriving (Show)

initialState :: ParserState
initialState = ParserState 
    { previousTokenIsLineTerminator = False }

setLineTerminatorState :: ParserState -> ParserState
setLineTerminatorState _ = ParserState True

clearLineTerminatorState :: ParserState -> ParserState
clearLineTerminatorState _ = ParserState False

----------------------

acceptAnyRawToken :: TokenParser LocatedToken
acceptAnyRawToken = token showTok posFromTok testTok
    where
        showTok t = show t
        posFromTok (LocatedToken _ pos) = pos
        testTok t = Just t

skipLeadingLineTerminators :: TokenParser a -> TokenParser a
skipLeadingLineTerminators p = (skipMany lineTerminatorToken) >> p

-- skip leading line terminators
acceptAnyToken :: TokenParser LocatedToken
acceptAnyToken = skipLeadingLineTerminators acceptAnyRawToken

identifierToken :: TokenParser String
identifierToken = try $ do
    t <- acceptAnyToken 
    case t of
        LocatedToken (IdentifierToken ident) _ -> do 
            updateState clearLineTerminatorState
            return ident
        _ -> fail "IdentifierToken"

identifierName :: TokenParser String
identifierName = 
    identifierToken 
    <|> identifierNameFromKeyword
    <|> identifierNameFromNullLiteral
    <|> identifierNameFromBoleanLiteral
    <?> "IdentifierName"

-- TODO: convert keyword to string
identifierNameFromKeyword :: TokenParser String
identifierNameFromKeyword = do
    _ <- anyKeywordToken
    return "keywordIdentifierName"

identifierNameFromNullLiteral :: TokenParser String
identifierNameFromNullLiteral = nullLiteralToken >> return "null"

-- TODO: convert bool to string
identifierNameFromBoleanLiteral :: TokenParser String
identifierNameFromBoleanLiteral = do
    _ <- booleanLiteralToken
    return "boolIdentifierName"

anyPunctuatorToken :: TokenParser Punctuator
anyPunctuatorToken = try $ do
    tok <- acceptAnyToken 
    case tok of
        LocatedToken (PunctuatorToken p) _ -> do
            updateState clearLineTerminatorState
            return p
        _ -> fail "KeywordToken"

punctuatorToken :: Punctuator -> TokenParser Punctuator
punctuatorToken p = try $ do
    x <- anyPunctuatorToken 
    if x == p 
        then return p 
        else fail $ "Punctuator " ++ show p

anyKeywordToken :: TokenParser Keyword
anyKeywordToken = try $ do
    tok <- acceptAnyToken 
    case tok of
        LocatedToken (KeywordToken key) _ -> do
            updateState clearLineTerminatorState
            return key
        _ -> fail "KeywordToken"

keywordToken :: Keyword -> TokenParser Keyword
keywordToken k = try $ do
    x <- anyKeywordToken 
    if x == k 
        then return k
        else fail $ "Keyword " ++ show k

nullLiteralToken :: TokenParser ()
nullLiteralToken = try $ do
    tok <- acceptAnyToken 
    case tok of
        LocatedToken NullLiteralToken _ -> do
            updateState clearLineTerminatorState
            return ()
        _ -> fail "NullLiteralToken"

booleanLiteralToken :: TokenParser Bool
booleanLiteralToken = try $ do
    tok <- acceptAnyToken 
    case tok of
        LocatedToken (BooleanLiteralToken bool) _ -> do
            updateState clearLineTerminatorState
            return bool
        _ -> fail "BooleanLiteralToken"

numericLiteralToken :: TokenParser Double
numericLiteralToken = try $ do
    tok <- acceptAnyToken 
    case tok of
        LocatedToken (NumericLiteralToken num) _ -> do
            updateState clearLineTerminatorState
            return num
        _ -> fail "NumericLiteralToken"

stringLiteralToken :: TokenParser String
stringLiteralToken = try $ do
    tok <- acceptAnyToken 
    case tok of
        LocatedToken (StringLiteralToken str) _ -> do
            updateState clearLineTerminatorState
            return str
        _ -> fail "StringLiteralToken"

lineTerminatorToken :: TokenParser ()
lineTerminatorToken = try $ do
    tok <- acceptAnyRawToken 
    case tok of
        LocatedToken LineTerminatorToken _ -> do
            updateState setLineTerminatorState
            return ()
        _ -> fail "LineTerminatorToken"

semicolon :: TokenParser Punctuator
semicolon = punctuatorToken SemicolonPunctuator

tokenIsRightCurlyBracket :: LocatedToken -> Bool
tokenIsRightCurlyBracket (LocatedToken (PunctuatorToken RightCurlyBracketPunctuator) _) = True
tokenIsRightCurlyBracket _ = False

autoSemicolon :: TokenParser Punctuator
autoSemicolon = skipLeadingLineTerminators $ do
    prevTokIsLT <- liftM previousTokenIsLineTerminator getState
    input <- getInput
    let emptyInput = null input
    let nextTokIsRightBrace = (not emptyInput) && (tokenIsRightCurlyBracket $ head input)
    let enableAutoSemi = prevTokIsLT || emptyInput || nextTokIsRightBrace
    if enableAutoSemi
        then option SemicolonPunctuator semicolon
        else semicolon
