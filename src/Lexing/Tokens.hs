module Lexing.Tokens where

data Token = 
    NumericLiteralToken Double
    | StringLiteralToken String
    | IdentifierToken String
    | KeywordToken Keyword
    | NullLiteralToken
    | BooleanLiteralToken Bool
    | CommentToken String
    | PunctuatorToken Punctuator
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

data Punctuator = 
    LeftCurlyBracketPunctuator
    | RightCurlyBracketPunctuator
    | LeftRoundBracketPunctuator
    | RightRoundBracketPunctuator
    | LeftSquareBracketPunctuator
    | RightSquareBracketPunctuator
    | DotPunctuator
    | SemicolonPunctuator
    | CommaPunctuator
    | LessThanPunctuator
    | GreaterThanPunctuator
    | LessThanEqualsPunctuator
    | GreaterThanEqualsPunctuator
    | EqualsPunctuator
    | NotEqualsPunctuator
    | StrictEqualsPunctuator
    | StrictNotEqualsPunctuator
    | PlusPunctuator
    | MinusPunctuator
    | MulPunctuator
    | ModulusPunctuator
    | IncrementPlusPunctuator
    | IncrementMinusPunctuator
    | LeftShiftPunctuator
    | RightShiftPunctuator
    | UnsignedRightShiftPunctuator
    | BitwiseAndPunctuator
    | BitwiseOrPunctuator
    | BitwiseXorPunctuator
    | LogicalNotPunctuator
    | BitwiseNotPunctuator
    | LogicalAndPunctuator
    | LogicalOrPunctuator
    | QuestionMarkPunctuator
    | ColonPunctuator
    | AssignPunctuator
    | PlusAssignPunctuator
    | MinusAssignPunctuator
    | MulAssignPunctuator
    | ModulusAssignPunctuator
    | LeftShiftAssignPunctuator
    | RightShiftAssignPunctuator
    | UnsignedRightShiftAssignPunctuator
    | BitwiseAndAssignPunctuator
    | BitwiseOrAssignPunctuator
    | BitwiseXorAssignPunctuator
    | DivPunctuator
    | DivAssignPunctuator
    deriving (Show)
