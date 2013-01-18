module ParserUserState where

import Debug.Trace
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import Control.Monad


data Number = Number1 | Number2
    deriving (Show, Eq)

data Token
    = SemicolonToken
    | NumberToken Number
    | PlusToken
    deriving (Show, Eq)

data Program = Program [Statement]
    deriving (Show, Eq)

data Statement = 
    ExpressionStatement Expression
    deriving (Show, Eq) 

data Expression
    = UnaryExpression Number
    | PlusExpression Number Number
    deriving (Show, Eq)


----------------

data ParserState = ParserState LineTerminatorState

type LineTerminatorState = Bool

clearLineTerminatorState :: ParserState -> ParserState
clearLineTerminatorState (ParserState _) = ParserState False

setLineTerminatorState :: ParserState -> ParserState
setLineTerminatorState (ParserState _) = ParserState True


type TokenParser a = GenParser Token ParserState a


-- Tokens

rawToken :: TokenParser Token
rawToken = token showTok posFromTok testTok
    where
        showTok t = show t
        posFromTok _ = initialPos "tokens source"
        testTok t = Just t

numberToken :: TokenParser ()
numberToken = try $ do
    t <- rawToken 
    case t of
        (NumberToken _) -> return ()
        _ -> fail "expecting number"

--
semicolonToken :: TokenParser ()
semicolonToken = try $ do
    t <- rawToken 
    case t of
        SemicolonToken -> return ()
        _ -> fail "expecting semicolon"

plusToken :: TokenParser ()
plusToken = try $ do
    t <- rawToken 
    case t of
        PlusToken -> return ()
        _ -> fail "expecting plus"

autoSemicolon :: TokenParser ()
autoSemicolon = do
    st <- getState
    case st of
        (ParserState True) -> (option () semicolonToken)
        (ParserState False) -> (semicolonToken)

-- Ast

program :: TokenParser Program
program = do
    ss <- many statement
    eof
    return $ Program ss

statement :: TokenParser Statement
statement = semicolonInserter $ do
    e <- expression
    autoSemicolon
    return $ ExpressionStatement e

expression :: TokenParser Expression
expression = try plusExpression <|> unaryExpression

unaryExpression :: TokenParser Expression
unaryExpression = do
    numberToken
    return $ UnaryExpression Number1

plusExpression :: TokenParser Expression
plusExpression = do
    numberToken >> plusToken >> numberToken
    return $ PlusExpression Number1 Number2


-----------------------

parseFromTokens :: [Token] -> Either ParseError Program
parseFromTokens input = runParser program (ParserState False) "tokens" input

semicolonInserter :: TokenParser a -> TokenParser a
semicolonInserter p = try p <|> do
    trace "setting state" (updateState setLineTerminatorState)
    res <- optionMaybe (p >> p)
    trace "reseting state" (updateState clearLineTerminatorState)
    case res of
        Nothing -> fail "not parsed"
        (Just x) -> return x


        
-- test inputs
-- We are expecting semicolon at the end of expression

-- "1;"
input1 = [NumberToken Number1, SemicolonToken]

-- "1 + 2;"
input2 = [NumberToken Number1, PlusToken, NumberToken Number1, SemicolonToken]

-- "1"
input3 = [NumberToken Number1]

-- "1 1 + 2;" - must be parsed as "1; 1 + 2;"
input4 = [NumberToken Number1, NumberToken Number1, PlusToken, NumberToken Number1, SemicolonToken]

input5 = [NumberToken Number1, PlusToken, NumberToken Number1]
