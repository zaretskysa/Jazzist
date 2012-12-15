module Parsing.Ast where

data Program = 
    Program [SourceElement] deriving (Show)

data SourceElement = 
    StatementSourceElement (Statement (LeftHandSideExpression SourceElement)  (Expression SourceElement) SourceElement)
    | FunctionDeclarationSourceElement String [String] (FunctionBody SourceElement)
    deriving (Show)

data Statement lhsExpr expression sourceElement =
    BlockStmt (Block lhsExpr expression sourceElement)
    | VariableStmt [(VariableDeclaration sourceElement)] -- TODO: use non-empty list
    | EmptyStmt
    | ExpressionStmt expression
    | IfStmt expression (Statement lhsExpr expression sourceElement) (MaybeStatement lhsExpr expression sourceElement)
    | IterationStmt (IterationStatement lhsExpr expression sourceElement)
    | ContinueStmt MaybeString
    | BreakStmt MaybeString
    | ReturnStmt (MaybeExpression sourceElement)
    | WithStmt expression (Statement lhsExpr expression sourceElement)
    | LabelledStmt String (Statement lhsExpr expression sourceElement)
    | SwitchStmt expression (CaseBlock lhsExpr expression sourceElement)
    | ThrowStmt expression
    | TryStmt (TryStatement lhsExpr expression sourceElement)
    | DebuggerStmt
    deriving (Show)

data Block lhsExpr expression sourceElement = 
    Block [(Statement lhsExpr expression sourceElement)] 
    deriving (Show)

type MaybeString = Maybe String

type MaybeStatement lhsExpr expression sourceElement = 
    Maybe (Statement lhsExpr expression sourceElement)

-- variable name and initializer
data VariableDeclaration sourceElement = 
    VariableDeclaration String (MaybeInitializer sourceElement)
    deriving (Show)

data Initializer sourceElement = 
    Initializer (AssignmentExpression sourceElement) 
    deriving (Show)

type MaybeInitializer sourceElement = Maybe (Initializer sourceElement)

-- TODO: separate while and for statements
-- TODO: use NoIn ?
data IterationStatement lhsExpr expression sourceElement =
    DoWhileIterationStatement (Statement lhsExpr expression sourceElement) expression
    | WhileIterationStatement expression (Statement lhsExpr expression sourceElement)
    | ExprTripletForIterationStatement (MaybeExpression sourceElement) (MaybeExpression sourceElement) (MaybeExpression sourceElement) (Statement lhsExpr expression sourceElement)
    | VarAndDoubleExprForIterationStatement [(VariableDeclaration sourceElement)] (MaybeExpression sourceElement) (MaybeExpression sourceElement) (Statement lhsExpr expression sourceElement) --use non empty list
    | LHSExprInExprForIterationStatement lhsExpr expression (Statement lhsExpr expression sourceElement)
    | VarInExprIteratioinStatement  (VariableDeclaration sourceElement) expression (Statement lhsExpr expression sourceElement)
    deriving (Show)

data ReturnStatement sourceElement =
    JustReturnStatement
    | ExpressionReturnStatement (Expression sourceElement)
    deriving (Show)

data CaseBlock lhsExpr expression sourceElement = 
    CaseBlock [(CaseClause lhsExpr expression sourceElement)] (MaybeDefaultClause lhsExpr expression sourceElement) [(CaseClause lhsExpr expression sourceElement)] 
    deriving (Show)

data CaseClause lhsExpr expression sourceElement = 
    CaseClause (Expression sourceElement) [(Statement lhsExpr expression sourceElement)] 
    deriving (Show)

data DefaultClause lhsExpr expression sourceElement = 
    DefaultClause [(Statement lhsExpr expression sourceElement)] 
    deriving (Show)

type MaybeDefaultClause lhsExpr expression sourceElement = 
    Maybe (DefaultClause lhsExpr expression sourceElement)

data TryStatement lhsExpr expression sourceElement = 
    BlockCatchTryStatement (Block lhsExpr expression sourceElement) (Catch lhsExpr expression sourceElement)
    | BlockFinallyTryStatement (Block lhsExpr expression sourceElement) (Finally lhsExpr expression sourceElement)
    | BlockCatchFinallyTryStatement (Block lhsExpr expression sourceElement) (Catch lhsExpr expression sourceElement) (Finally lhsExpr expression sourceElement)
    deriving (Show)

data Catch lhsExpr expression sourceElement = Catch String (Block lhsExpr expression sourceElement) deriving (Show)

data Finally lhsExpr expression sourceElement = 
    Finally (Block lhsExpr expression sourceElement) 
    deriving (Show)


-- Assignment expression

data AssignmentExpression sourceElement = 
    ConditionalAssignmentExpression (ConditionalExpression sourceElement)
    | AssignmentOperatorExpression (LeftHandSideExpression sourceElement) AssignmentOperator (AssignmentExpression sourceElement)
    deriving (Show)

type MaybeAssignmentExpression sourceElement = Maybe (AssignmentExpression sourceElement)

data AssignmentOperator =
    SingleAssignOperator
    | MulAssignOperator
    | DivAssignOperator
    | ModulusAssignOperator
    | PlusAssignOperator
    | MinusAssignOperator
    | LeftShiftAssignOperator
    | RightShiftAssignOperator
    | UnsignedRightShiftAssignOperator
    | BitwiseAndAssignOperator
    | BitwiseXorAssignOperator
    | BitwiseOrAssignOperator
    deriving (Show)

data ConditionalExpression sourceElement = 
    LogicalOrConditionalExpression (LogicalOrExpression sourceElement)
    | TeranaryOperatorConditionalExpression (LogicalOrExpression sourceElement) (AssignmentExpression sourceElement) (AssignmentExpression sourceElement)
    deriving (Show)

--data AssignmentExpressionNoIn = 
--    ConditionalAssignmentExpressionNoIn ConditionalExpressionNoIn
--    | AssignmentOperatorExpression LeftHandSideExpression AssignmentOperator AssignmentExpressionNoIn
--    deriving (Show)
--
--type MaybeAssignmentExpressionNoIn = Maybe AssignmentExpressionNoIn
--
--data ConditionalExpressionNoIn = 
--    LogicalOrConditionalExpressionNoIn LogicalOrExpressionNoIn
--    | TeranaryOperatorConditionalExpressionNoIn LogicalOrExpressionNoIn AssignmentExpression AssignmentExpressionNoIn
--    deriving (Show)

-- Left hand side expressions

data MemberExpression sourceElement = 
    DummyMemberExpression
    | PrimaryMemberExpression (PrimaryExpression sourceElement)
    | FunctionMemberExpression (FunctionExpression sourceElement)
    | PropertyAccessByBracketsMemberExpression (MemberExpression sourceElement) (Expression sourceElement)
    | PropertyAccessByDotMemberExpression (MemberExpression sourceElement) String
    | NewMemberExpression (MemberExpression sourceElement) [(AssignmentExpression sourceElement)]
    deriving (Show)

data FunctionExpression sourceElement = 
    FunctionExpression MaybeString [String] (FunctionBody sourceElement)
    deriving (Show)

data NewExpression sourceElement =
    MemberNewExpression (MemberExpression sourceElement)
    | NewNewExpression (NewExpression sourceElement)
    deriving (Show)

data CallExpression sourceElement =
    MemberWithArgumentsCallExpression (MemberExpression sourceElement) [(AssignmentExpression sourceElement)]
    | CallWithArgumentsCallExpression (CallExpression sourceElement) [(AssignmentExpression sourceElement)]
    | PropertyAccessByBracketsCallExpression (CallExpression sourceElement) (Expression sourceElement)
    | PropertyAccessByDotCallExpression (CallExpression sourceElement) String
    deriving (Show)

data LeftHandSideExpression sourceElement =
    NewLHSExpression (NewExpression sourceElement)
    | CallLHSExpression (CallExpression sourceElement)
    deriving (Show)

 -- Primary expression

data PrimaryExpression sourceElement = 
    ThisPrimaryExpression
    | IdentifierPrimaryExpression String
    | LiteralPrimaryExpression Literal
    | ArrayLiteralPrimaryExpression (ArrayLiteral sourceElement)
    | ObjectLiteralPrimaryExpression (ObjectLiteral sourceElement)
    | ExpressionPrimaryExpression (Expression sourceElement)
    deriving (Show)

data ObjectLiteral sourceElement = ObjectLiteral [(PropertyAssignment sourceElement)] deriving (Show)

data ArrayLiteral sourceElement = ArrayLiteral [(MaybeAssignmentExpression sourceElement)] deriving (Show)

data Literal = 
    NullLiteral
    | BooleanLiteral Bool
    | NumericLiteral Double
    | StringLiteral String
--  | RegularExpressionLiteral TODO
    deriving (Show)

data PropertyAssignment sourceElement =
    FieldPropertyAssignment PropertyName (AssignmentExpression sourceElement)
    | GetterPropertyAssignment PropertyName (FunctionBody sourceElement)
    | SetterPropertyAssignment PropertyName PropertySetParameterList (FunctionBody sourceElement)
    deriving (Show)

type PropertySetParameterList = String

data PropertyName = 
    StringPropertyName String
    | NumericPropertyName Double
    deriving (Show)

data FunctionBody sourceElement = FunctionBody [sourceElement] deriving (Show)

-- Logical Expressions

data LogicalAndExpression sourceElement = 
    UnaryLogicalAndExpression (BitwiseOrExpression sourceElement)
    | BinaryLogicalAndExpression (LogicalAndExpression sourceElement) (BitwiseOrExpression sourceElement)
    deriving (Show)

data LogicalOrExpression sourceElement =
    UnaryLogicalOrExpression (LogicalAndExpression sourceElement)
    | BinaryLogicalOrExpression (LogicalOrExpression sourceElement) (LogicalAndExpression sourceElement)
    deriving (Show)

-- Comma operator

data Expression sourceElement = 
    Expression [(AssignmentExpression sourceElement)] -- TODO: non empty
    deriving (Show)

type MaybeExpression sourceElement = Maybe (Expression sourceElement)

--data ExpressionNoIn =
--    ExpressionNoIn [AssignmentExpressionNoIn]
--    deriving (Show)
--
--type MaybeExpressionNoIn = Maybe ExpressionNoIn

-- Binary bitwise operators

data BitwiseAndExpression sourceElement = 
    UnaryBitwiseAndExpression (EqualityExpression sourceElement)
    | BinaryBitwiseAndExpression (BitwiseAndExpression sourceElement) (EqualityExpression sourceElement)
    deriving (Show)

data BitwiseXorExpression sourceElement =
    UnaryBitwiseXorExpression (BitwiseAndExpression sourceElement)
    | BinaryBitwiseXorExpression (BitwiseXorExpression sourceElement) (BitwiseAndExpression sourceElement)
    deriving (Show)

data BitwiseOrExpression sourceElement =
    UnaryBitwiseOrExpression (BitwiseXorExpression sourceElement)
    | BinaryBitwiseOrExpression (BitwiseOrExpression sourceElement) (BitwiseXorExpression sourceElement)
    deriving (Show)

-- Equality operators

data EqualityExpression sourceElement =
    RelationalEqualityExpression (RelationalExpression sourceElement)
    | EqualsEqualityExpression (EqualityExpression sourceElement) (RelationalExpression sourceElement)
    | NotEqualsEqualityExpression (EqualityExpression sourceElement) (RelationalExpression sourceElement)
    | StrictEqualsEqualityExpression (EqualityExpression sourceElement) (RelationalExpression sourceElement)
    | StrictNotEqualsEqualityExpression (EqualityExpression sourceElement) (RelationalExpression sourceElement)
    deriving (Show)

-- Relational operators

data RelationalExpression sourceElement =
    ShiftRelationalExpression (ShiftExpression sourceElement)
    | LessThanRelationalExpression (RelationalExpression sourceElement) (ShiftExpression sourceElement)
    | GreaterThanRelationalExpression (RelationalExpression sourceElement) (ShiftExpression sourceElement)
    | LessThanEqualsRelationalExpression (RelationalExpression sourceElement) (ShiftExpression sourceElement)
    | GreaterThanEqualsRelationalExpression (RelationalExpression sourceElement) (ShiftExpression sourceElement)
    | InstanceOfRelationalExpression (RelationalExpression sourceElement) (ShiftExpression sourceElement)
    | InRelationalExpression (RelationalExpression sourceElement) (ShiftExpression sourceElement)
    deriving (Show)

-- Bitwise shift operators

data ShiftExpression sourceElement = 
    AdditiveShiftExpression (AdditiveExpression sourceElement)
    | LeftShiftExpression (ShiftExpression sourceElement) (AdditiveExpression sourceElement)
    | RightShiftExpression (ShiftExpression sourceElement) (AdditiveExpression sourceElement)
    | UnsignedRightShiftExpression (ShiftExpression sourceElement) (AdditiveExpression sourceElement)
    deriving (Show)

-- Additive operators

data AdditiveExpression sourceElement = 
    MultAdditiveExpression (MultiplicativeExpression sourceElement)
    | PlusAdditiveExpression (AdditiveExpression sourceElement) (MultiplicativeExpression sourceElement)
    | MinusAdditiveExpression (AdditiveExpression sourceElement) (MultiplicativeExpression sourceElement)
    deriving (Show)

-- Multiplicative operators

data MultiplicativeExpression sourceElement =
    UnaryMultiplicativeExpression (UnaryExpression sourceElement)
    | MulMultiplicativeExpression (MultiplicativeExpression sourceElement) (UnaryExpression sourceElement)
    | DivMultiplicativeExpression (MultiplicativeExpression sourceElement) (UnaryExpression sourceElement)
    | ModulusMultiplicativeExpression (MultiplicativeExpression sourceElement) (UnaryExpression sourceElement)
    deriving (Show)

-- Unary operators

data UnaryExpression sourceElement =
    PostfixUnaryExpression (PostfixExpression sourceElement)
    | DeleteUnaryExpression (UnaryExpression sourceElement)
    | VoidUnaryExpression (UnaryExpression sourceElement)
    | TypeOfUnaryExpression (UnaryExpression sourceElement)
    | IncrementPlusUnaryExpression (UnaryExpression sourceElement)
    | IncrementMinusUnaryExpression (UnaryExpression sourceElement)
    | PlusUnaryExpression (UnaryExpression sourceElement)
    | MinusUnaryExpression (UnaryExpression sourceElement)
    | BitwiseNotUnaryExpression (UnaryExpression sourceElement)
    | LogicalNotUnaryExpression (UnaryExpression sourceElement)
    deriving (Show)

-- Postfix expressions

data PostfixExpression sourceElement =
    LHSPostfixExpression (LeftHandSideExpression sourceElement)
    | IncrementPlusPostfixExpression (LeftHandSideExpression sourceElement)
    | IncrementMinusPostfixExpression (LeftHandSideExpression sourceElement)
    deriving (Show)



