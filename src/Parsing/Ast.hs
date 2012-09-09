module Parsing.Ast where

data Program = SourceElements [SourceElement]

data SourceElement = 
    Statement
--  | FunctionDeclaration

data Statement =
    Block
--    | VariableStatement
--    | EmptyStatement
--    | ExpressionStatement
--    | IfStatement
--    | IterationStatement
--    | ContinueStatement
--    | BreakStatement
--    | ReturnStatement
--    | WithStatement
--    | LabelledStatement
--    | SwitchStatement
--    | ThrowStatement
--    | TryStatement
--    | DebuggerStatement
    deriving (Show)