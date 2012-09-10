module Parsing.Data.Statement where

import Parsing.Data.Expression

data Statement =
    BlockStmt Block
    | VariableStmt [VariableDeclaration] -- TODO: use non-empty list
    | EmptyStmt
    | ExpressionStmt Expression
    | IfStmt Expression Statement MaybeStatement
    | IterationStmt IterationStatement
    | ContinueStmt MaybeString
    | BreakStmt MaybeString
    | ReturnStmt MaybeExpression
    | WithStmt Expression Statement
    | LabelledStmt String Statement
    | SwitchStmt Expression CaseBlock
    | ThrowStmt Expression
    | TryStmt TryStatement
    | DebuggerStmt
    deriving (Show)

data Block = Block [Statement] deriving (Show)

type MaybeString = Maybe String

type MaybeStatement = Maybe Statement

-- variable name and initializer
data VariableDeclaration = 
    VariableDeclaration String MaybeAssignmentExpression
    deriving (Show)

-- TODO: separate while and for statements
-- TODO: use NoIn ?
data IterationStatement =
    DoWhileIterationStatement Statement Expression
    | WhileIterationStatement Expression Statement
    | ExprTripletForIterationStatement MaybeExpression MaybeExpression MaybeExpression Statement
    | VarAndDoubleExprForIterationStatement [VariableDeclaration] MaybeExpression MaybeExpression Statement --use non empty list
    | LHSExprInExprForIterationStatement LeftHandSideExpression Expression Statement
    | VarInExprIteratioinStatement  VariableDeclaration Expression Statement
    deriving (Show)

data ReturnStatement =
    JustReturnStatement
    | ExpressionReturnStatement Expression
    deriving (Show)

data CaseBlock = 
    CaseBlock [CaseClause] MaybeDefaultClause [CaseClause] 
    deriving (Show)

data CaseClause = 
    CaseClause Expression [Statement] deriving (Show)

data DefaultClause = DefaultCluase [Statement] deriving (Show)

type MaybeDefaultClause = Maybe DefaultClause

data TryStatement = 
    BlockCatchTryStatement Block Catch
    | BlockFinnalyTryStatement Block Finnaly
    | BlockCatchFinnalyTryStatement Block Catch Finnaly
    deriving (Show)

data Catch = Catch String Block deriving (Show)

data Finnaly = Finnaly Block deriving (Show)


