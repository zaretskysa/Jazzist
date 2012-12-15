module Parsing.Statement where

data Statement assignExpr lhsExpr expression sourceElement =
    BlockStmt (Block assignExpr lhsExpr expression sourceElement)
    | VariableStmt [(VariableDeclaration assignExpr sourceElement)] -- TODO: use non-empty list
    | EmptyStmt
    | ExpressionStmt expression
    | IfStmt expression (Statement assignExpr lhsExpr expression sourceElement) (MaybeStatement assignExpr lhsExpr expression sourceElement)
    | IterationStmt (IterationStatement assignExpr lhsExpr expression sourceElement)
    | ContinueStmt (Maybe String)
    | BreakStmt (Maybe String)
    | ReturnStmt (MaybeExpression expression)
    | WithStmt expression (Statement assignExpr lhsExpr expression sourceElement)
    | LabelledStmt String (Statement assignExpr lhsExpr expression sourceElement)
    | SwitchStmt expression (CaseBlock assignExpr lhsExpr expression sourceElement)
    | ThrowStmt expression
    | TryStmt (TryStatement assignExpr lhsExpr expression sourceElement)
    | DebuggerStmt
    deriving (Show)

data Block assignExpr lhsExpr expression sourceElement = 
    Block [(Statement assignExpr lhsExpr expression sourceElement)] 
    deriving (Show)

-- variable name and initializer
data VariableDeclaration assignExpr sourceElement = 
    VariableDeclaration String (MaybeInitializer assignExpr)
    deriving (Show)

data Initializer assignExpr = 
    Initializer assignExpr
    deriving (Show)

type MaybeInitializer assignExpr = Maybe (Initializer assignExpr)

type MaybeStatement assignExpr lhsExpr expression sourceElement = 
    Maybe (Statement assignExpr lhsExpr expression sourceElement)

-- TODO: separate while and for statements
-- TODO: use NoIn ?
data IterationStatement assignExpr lhsExpr expression sourceElement =
    DoWhileIterationStatement (Statement assignExpr lhsExpr expression sourceElement) expression
    | WhileIterationStatement expression (Statement assignExpr lhsExpr expression sourceElement)
    | ExprTripletForIterationStatement (MaybeExpression expression) (MaybeExpression expression) (MaybeExpression expression) (Statement assignExpr lhsExpr expression sourceElement)
    | VarAndDoubleExprForIterationStatement [(VariableDeclaration assignExpr sourceElement)] (MaybeExpression expression) (MaybeExpression expression) (Statement assignExpr lhsExpr expression sourceElement) --use non empty list
    | LHSExprInExprForIterationStatement lhsExpr expression (Statement assignExpr lhsExpr expression sourceElement)
    | VarInExprIteratioinStatement  (VariableDeclaration assignExpr sourceElement) expression (Statement assignExpr lhsExpr expression sourceElement)
    deriving (Show)

type MaybeExpression expression = Maybe expression

--data ExpressionNoIn =
--    ExpressionNoIn [AssignmentExpressionNoIn]
--    deriving (Show)
--
--type MaybeExpressionNoIn = Maybe ExpressionNoIn

data CaseBlock assignExpr lhsExpr expression sourceElement = 
    CaseBlock [(CaseClause assignExpr lhsExpr expression sourceElement)] (MaybeDefaultClause assignExpr lhsExpr expression sourceElement) [(CaseClause assignExpr lhsExpr expression sourceElement)] 
    deriving (Show)

data CaseClause assignExpr lhsExpr expression sourceElement = 
    CaseClause expression [(Statement assignExpr lhsExpr expression sourceElement)] 
    deriving (Show)

data DefaultClause assignExpr lhsExpr expression sourceElement = 
    DefaultClause [(Statement assignExpr lhsExpr expression sourceElement)] 
    deriving (Show)

type MaybeDefaultClause assignExpr lhsExpr expression sourceElement = 
    Maybe (DefaultClause assignExpr lhsExpr expression sourceElement)

data TryStatement assignExpr lhsExpr expression sourceElement = 
    BlockCatchTryStatement (Block assignExpr lhsExpr expression sourceElement) (Catch assignExpr lhsExpr expression sourceElement)
    | BlockFinallyTryStatement (Block assignExpr lhsExpr expression sourceElement) (Finally assignExpr lhsExpr expression sourceElement)
    | BlockCatchFinallyTryStatement (Block assignExpr lhsExpr expression sourceElement) (Catch assignExpr lhsExpr expression sourceElement) (Finally assignExpr lhsExpr expression sourceElement)
    deriving (Show)

data Catch assignExpr lhsExpr expression sourceElement = Catch String (Block assignExpr lhsExpr expression sourceElement) deriving (Show)

data Finally assignExpr lhsExpr expression sourceElement = 
    Finally (Block assignExpr lhsExpr expression sourceElement) 
    deriving (Show)
