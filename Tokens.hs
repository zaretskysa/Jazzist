module Tokens where

data Token = 
    NumericLiteralToken Double
    | StringLiteralToken String
    | IdentifierToken String
    | KeywordToken Keyword
    | NullLiteralToken
    | BooleanLiteralToken Bool
    | CommentToken String
    deriving (Show)

data Keyword = 
    BreakKeyword
    | CaseKeyword
    | CatchKeyword
    | ContinueKeyword
    | DebuggerKeyword
    | DefaultKeyword
    | DeleteKeyword
    | DoKeyword
    | ElseKeyword
    | FinallyKeyword
    | ForKeyword
    | FunctionKeyword
    | IfKeyword
    | InKeyword
    | InstanceOfKeyword
    | NewKeyword
    | ReturnKeyword
    | SwitchKeyword
    | ThisKeyword
    | ThrowKeyword
    | TryKeyword
    | TypeOfKeyword
    | VarKeyword
    | VoidKeyword
    | WhileKeyword
    | WithKeyword
    deriving (Show)
