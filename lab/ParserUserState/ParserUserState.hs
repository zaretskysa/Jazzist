module ParserUserState where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos


data Number = Number1 | Number2
    deriving (Show, Eq)

data Token
    = SemicolonToken
    | NumberToken Number
    | PlusToken
    deriving (Show, Eq)

data Program = Program [Expression]
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

--
autoSemicolonToken :: TokenParser ()
autoSemicolonToken = option () semicolonToken >> return ()

plusToken :: TokenParser ()
plusToken = try $ do
    t <- rawToken 
    case t of
        PlusToken -> return ()
        _ -> fail "expecting plus"


-- Ast

program :: TokenParser Program
program = do
    es <- many expression
    eof
    return $ Program es

expression :: TokenParser Expression
expression = try unaryExpression <|> plusExpression

unaryExpression :: TokenParser Expression
unaryExpression = 
    numberToken >> autoSemicolonToken >> 
    (return $ UnaryExpression Number1)

plusExpression :: TokenParser Expression
plusExpression = 
    numberToken >> plusToken >> numberToken >> autoSemicolonToken >> 
    (return $ PlusExpression Number1 Number2)


-----------------------

parseFromTokens :: [Token] -> Either ParseError Program
parseFromTokens input = runParser program (ParserState False) "tokens" input


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

