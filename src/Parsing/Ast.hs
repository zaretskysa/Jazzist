module Parsing.Ast where

data Program = 
    Program [SourceElement] deriving (Show, Eq)

data SourceElement = 
    StatementSourceElement Statement
    | FunctionDeclarationSourceElement String [String] FunctionBody
    deriving (Show, Eq)

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
    deriving (Show, Eq)

data Block = Block [Statement] deriving (Show, Eq)

type MaybeString = Maybe String

type MaybeStatement = Maybe Statement

-- variable name and initializer
data VariableDeclaration = 
    VariableDeclaration String MaybeInitializer
    deriving (Show, Eq)

data Initializer = 
    Initializer AssignmentExpression 
    deriving (Show, Eq)

type MaybeInitializer = Maybe Initializer

-- TODO: separate while and for statements
-- TODO: use NoIn ?
data IterationStatement =
    DoWhileIterationStatement Statement Expression
    | WhileIterationStatement Expression Statement
    | ExprTripletForIterationStatement MaybeExpression MaybeExpression MaybeExpression Statement
    | VarAndDoubleExprForIterationStatement [VariableDeclaration] MaybeExpression MaybeExpression Statement --use non empty list
    | LHSExprInExprForIterationStatement LeftHandSideExpression Expression Statement
    | VarInExprIteratioinStatement VariableDeclaration Expression Statement
    deriving (Show, Eq)

data ReturnStatement =
    JustReturnStatement
    | ExpressionReturnStatement Expression
    deriving (Show, Eq)

data CaseBlock = 
    CaseBlock [CaseClause] MaybeDefaultClause [CaseClause] --TODO: merge Case Clauses ?
    deriving (Show, Eq)

data CaseClause = 
    CaseClause Expression [Statement] deriving (Show, Eq)

data DefaultClause = DefaultClause [Statement] deriving (Show, Eq)

type MaybeDefaultClause = Maybe DefaultClause

data TryStatement = 
    BlockCatchTryStatement Block Catch
    | BlockFinallyTryStatement Block Finally
    | BlockCatchFinallyTryStatement Block Catch Finally
    deriving (Show, Eq)

data Catch = Catch String Block deriving (Show, Eq)

data Finally = Finally Block deriving (Show, Eq)


-- Assignment expression

data AssignmentExpression = 
    ConditionalAssignmentExpression ConditionalExpression
    | AssignmentOperatorExpression LeftHandSideExpression AssignmentOperator AssignmentExpression
    deriving (Show, Eq)

type MaybeAssignmentExpression = Maybe AssignmentExpression

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
    deriving (Show, Eq)

data ConditionalExpression = 
    LogicalOrConditionalExpression LogicalOrExpression --TODO: rename to UnaryConditionalExpression
    | TeranaryOperatorConditionalExpression LogicalOrExpression AssignmentExpression AssignmentExpression
    deriving (Show, Eq)

--data AssignmentExpressionNoIn = 
--    ConditionalAssignmentExpressionNoIn ConditionalExpressionNoIn
--    | AssignmentOperatorExpression LeftHandSideExpression AssignmentOperator AssignmentExpressionNoIn
--    deriving (Show, Eq)
--
--type MaybeAssignmentExpressionNoIn = Maybe AssignmentExpressionNoIn
--
--data ConditionalExpressionNoIn = 
--    LogicalOrConditionalExpressionNoIn LogicalOrExpressionNoIn
--    | TeranaryOperatorConditionalExpressionNoIn LogicalOrExpressionNoIn AssignmentExpression AssignmentExpressionNoIn
--    deriving (Show, Eq)

-- Left hand side expressions

data MemberExpression = 
    PrimaryMemberExpression PrimaryExpression
    | FunctionMemberExpression FunctionExpression
    | PropertyAccessByBracketsMemberExpression MemberExpression Expression
    | PropertyAccessByDotMemberExpression MemberExpression String
    | NewMemberExpression MemberExpression [AssignmentExpression]
    deriving (Show, Eq)

data FunctionExpression = 
    FunctionExpression MaybeString [String] FunctionBody
    deriving (Show, Eq)

data NewExpression =
    MemberNewExpression MemberExpression
    | NewNewExpression NewExpression
    deriving (Show, Eq)

data CallExpression =
    MemberWithArgumentsCallExpression MemberExpression [AssignmentExpression]
    | CallWithArgumentsCallExpression CallExpression [AssignmentExpression]
    | PropertyAccessByBracketsCallExpression CallExpression Expression
    | PropertyAccessByDotCallExpression CallExpression String
    deriving (Show, Eq)

data LeftHandSideExpression =
    NewLHSExpression NewExpression
    | CallLHSExpression CallExpression
    deriving (Show, Eq)

 -- Primary expression

data PrimaryExpression = 
    ThisPrimaryExpression
    | IdentifierPrimaryExpression String
    | LiteralPrimaryExpression Literal
    | ArrayLiteralPrimaryExpression ArrayLiteral
    | ObjectLiteralPrimaryExpression ObjectLiteral
    | ExpressionPrimaryExpression Expression
    deriving (Show, Eq)

data ObjectLiteral = ObjectLiteral [PropertyAssignment] deriving (Show, Eq)

data ArrayLiteral = ArrayLiteral [MaybeAssignmentExpression] deriving (Show, Eq)

data Literal = 
    NullLiteral
    | BooleanLiteral Bool
    | NumericLiteral Double
    | StringLiteral String
--  | RegularExpressionLiteral TODO
    deriving (Show, Eq)

data PropertyAssignment =
    FieldPropertyAssignment PropertyName AssignmentExpression
    | GetterPropertyAssignment PropertyName FunctionBody
    | SetterPropertyAssignment PropertyName PropertySetParameterList FunctionBody
    deriving (Show, Eq)

type PropertySetParameterList = String

data PropertyName = 
    StringPropertyName String
    | NumericPropertyName Double
    deriving (Show, Eq)

data FunctionBody = FunctionBody [SourceElement] deriving (Show, Eq)

-- Logical Expressions

data LogicalAndExpression = 
    UnaryLogicalAndExpression BitwiseOrExpression
    | BinaryLogicalAndExpression LogicalAndExpression BitwiseOrExpression
    deriving (Show, Eq)

data LogicalOrExpression =
    UnaryLogicalOrExpression LogicalAndExpression
    | BinaryLogicalOrExpression LogicalOrExpression LogicalAndExpression
    deriving (Show, Eq)

-- Comma operator

data Expression = 
    Expression [AssignmentExpression] -- TODO: non empty
    deriving (Show, Eq)

type MaybeExpression = Maybe Expression

--data ExpressionNoIn =
--    ExpressionNoIn [AssignmentExpressionNoIn]
--    deriving (Show, Eq)
--
--type MaybeExpressionNoIn = Maybe ExpressionNoIn

-- Binary bitwise operators

data BitwiseAndExpression = 
    UnaryBitwiseAndExpression EqualityExpression
    | BinaryBitwiseAndExpression BitwiseAndExpression EqualityExpression
    deriving (Show, Eq)

data BitwiseXorExpression =
    UnaryBitwiseXorExpression BitwiseAndExpression
    | BinaryBitwiseXorExpression BitwiseXorExpression BitwiseAndExpression
    deriving (Show, Eq)

data BitwiseOrExpression =
    UnaryBitwiseOrExpression BitwiseXorExpression
    | BinaryBitwiseOrExpression BitwiseOrExpression BitwiseXorExpression
    deriving (Show, Eq)

-- Equality operators

data EqualityExpression =
    RelationalEqualityExpression RelationalExpression
    | EqualsEqualityExpression EqualityExpression RelationalExpression
    | NotEqualsEqualityExpression EqualityExpression RelationalExpression
    | StrictEqualsEqualityExpression EqualityExpression RelationalExpression
    | StrictNotEqualsEqualityExpression EqualityExpression RelationalExpression
    deriving (Show, Eq)

-- Relational operators

data RelationalExpression =
    ShiftRelationalExpression ShiftExpression
    | LessThanRelationalExpression RelationalExpression ShiftExpression
    | GreaterThanRelationalExpression RelationalExpression ShiftExpression
    | LessThanEqualsRelationalExpression RelationalExpression ShiftExpression
    | GreaterThanEqualsRelationalExpression RelationalExpression ShiftExpression
    | InstanceOfRelationalExpression RelationalExpression ShiftExpression
    | InRelationalExpression RelationalExpression ShiftExpression
    deriving (Show, Eq)

-- Bitwise shift operators

data ShiftExpression = 
    AdditiveShiftExpression AdditiveExpression --TODO: rename to UnaryShiftExpression ?
    | LeftShiftExpression ShiftExpression AdditiveExpression
    | RightShiftExpression ShiftExpression AdditiveExpression
    | UnsignedRightShiftExpression ShiftExpression AdditiveExpression
    deriving (Show, Eq)

-- Additive operators

data AdditiveExpression = 
    MultAdditiveExpression MultiplicativeExpression
    | PlusAdditiveExpression AdditiveExpression MultiplicativeExpression
    | MinusAdditiveExpression AdditiveExpression MultiplicativeExpression
    deriving (Show, Eq)

-- Multiplicative operators

data MultiplicativeExpression =
    UnaryMultiplicativeExpression UnaryExpression
    | MulMultiplicativeExpression MultiplicativeExpression UnaryExpression
    | DivMultiplicativeExpression MultiplicativeExpression UnaryExpression
    | ModulusMultiplicativeExpression MultiplicativeExpression UnaryExpression
    deriving (Show, Eq)

-- Unary operators

data UnaryExpression =
    PostfixUnaryExpression PostfixExpression
    | DeleteUnaryExpression UnaryExpression
    | VoidUnaryExpression UnaryExpression
    | TypeOfUnaryExpression UnaryExpression
    --TODO: rename IncrementPlus to Increment (Decrement)
    | IncrementPlusUnaryExpression UnaryExpression
    | IncrementMinusUnaryExpression UnaryExpression
    | PlusUnaryExpression UnaryExpression
    | MinusUnaryExpression UnaryExpression
    | BitwiseNotUnaryExpression UnaryExpression
    | LogicalNotUnaryExpression UnaryExpression
    deriving (Show, Eq)

-- Postfix expressions

data PostfixExpression =
    LHSPostfixExpression LeftHandSideExpression
    | IncrementPlusPostfixExpression LeftHandSideExpression
    | IncrementMinusPostfixExpression LeftHandSideExpression
    deriving (Show, Eq)
