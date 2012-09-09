module Parsing.Data.Statement where

import Parsing.Data.Expression

data Statement =
    BlockStatement [Statement]
    | VariableStatement [VariableDeclaration] -- TODO: use non-empty list
    | EmptyStatement
    | ExpressionStatement Expression
    | IfStatement
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

-- variable name and initializer
data VariableDeclaration = 
    VariableDeclaration String (Maybe AssignmentExpression)
    deriving (Show)
