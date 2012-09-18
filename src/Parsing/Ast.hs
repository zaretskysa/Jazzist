module Parsing.Ast where

data Program = 
    Program [SourceElement] deriving (Show)

data SourceElement = 
    StatementSourceElement Statement
    | FunctionDeclarationSourceElement String [String] FunctionBody
    deriving (Show)

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
    VariableDeclaration String MaybeInitializer
    deriving (Show)

data Initializer = 
    Initializer AssignmentExpression 
    deriving (Show)

type MaybeInitializer = Maybe Initializer

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


-- Assignment expression

data AssignmentExpression = 
    ConditionalAssignmentExpression ConditionalExpression
    | AssignmentOperatorExpression LeftHandSideExpression AssignmentOperator AssignmentExpression
    deriving (Show)

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
    deriving (Show)

data ConditionalExpression = 
    LogicalOrConditionalExpression LogicalOrExpression
    | TeranaryOperatorConditionalExpression LogicalOrExpression AssignmentExpression AssignmentExpression
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

data MemberExpression = 
    DummyMemberExpression
    | PrimaryMemberExpression PrimaryExpression
    | FunctionMemberExpression FunctionExpression
    | PropertyAccessByBracketsMemberExpression MemberExpression Expression
    | PropertyAccessByDotMemberExpression MemberExpression String
    | NewMemberExpression MemberExpression [AssignmentExpression]
    deriving (Show)

data FunctionExpression = 
    FunctionExpression MaybeString [String] FunctionBody
    deriving (Show)

data NewExpression =
    MemberNewExpression MemberExpression
    | NewNewExpression NewExpression
    deriving (Show)

data CallExpression =
    MemberWithArgumentsCallExpression MemberExpression [AssignmentExpression]
    | CallWithArgumentsCallExpression CallExpression [AssignmentExpression]
    | PropertyAccessByBracketsCallExpression CallExpression Expression
    | PropertyAccessByDotCallExpression CallExpression String
    deriving (Show)

data LeftHandSideExpression =
    NewLHSExpression NewExpression
    | CallLHSExpression CallExpression
    deriving (Show)

 -- Primary expression

data PrimaryExpression = 
    ThisPrimaryExpression
    | IdentifierPrimaryExpression String
    | LiteralPrimaryExpression Literal
    | ArrayLiteralPrimaryExpression ArrayLiteral
    | ObjectLiteralPrimaryExpression ObjectLiteral
    | ExpressionPrimaryExpression Expression
    deriving (Show)

data ObjectLiteral = ObjectLiteral [PropertyAssignment] deriving (Show)

data ArrayLiteral = ArrayLiteral [MaybeAssignmentExpression] deriving (Show)

data Literal = 
    NullLiteral
    | BooleanLiteral Bool
    | NumericLiteral Double
    | StringLiteral String
--  | RegularExpressionLiteral TODO
    deriving (Show)

data PropertyAssignment =
    FieldPropertyAssignment PropertyName AssignmentExpression
    | GetterPropertyAssignment PropertyName FunctionBody
    | SetterPropertyAssignment PropertyName PropertySetParameterList FunctionBody
    deriving (Show)

type PropertySetParameterList = String

data PropertyName = 
    StringPropertyName String
    | NumericPropertyName Double
    deriving (Show)

data FunctionBody = FunctionBody [SourceElement] deriving (Show)

-- Logical Expressions

data LogicalAndExpression = 
    UnaryLogicalAndExpression BitwiseOrExpression
    | BinaryLogicalAndExpression LogicalAndExpression BitwiseOrExpression
    deriving (Show)

data LogicalOrExpression =
    UnaryLogicalOrExpression LogicalAndExpression
    | BinaryLogicalOrExpression LogicalOrExpression LogicalAndExpression
    deriving (Show)

-- Comma operator

data Expression = 
    Expression [AssignmentExpression] -- TODO: non empty
    deriving (Show)

type MaybeExpression = Maybe Expression

--data ExpressionNoIn =
--    ExpressionNoIn [AssignmentExpressionNoIn]
--    deriving (Show)
--
--type MaybeExpressionNoIn = Maybe ExpressionNoIn

-- Binary bitwise operators

data BitwiseAndExpression = 
    UnaryBitwiseAndExpression EqualityExpression
    | BinaryBitwiseAndExpression BitwiseAndExpression EqualityExpression
    deriving (Show)

data BitwiseXorExpression =
    UnaryBitwiseXorExpression BitwiseAndExpression
    | BinaryBitwiseXorExpression BitwiseXorExpression BitwiseAndExpression
    deriving (Show)

data BitwiseOrExpression =
    UnaryBitwiseOrExpression BitwiseXorExpression
    | BinaryBitwiseOrExpression BitwiseOrExpression BitwiseXorExpression
    deriving (Show)

-- Equality operators

data EqualityExpression =
    RelationalEqualityExpression RelationalExpression
    | EqualsEqualityExpression EqualityExpression RelationalExpression
    | NotEqualsEqualityExpression EqualityExpression RelationalExpression
    | StrictEqualsEqualityExpression EqualityExpression RelationalExpression
    | StrictNotEqualsEqualityExpression EqualityExpression RelationalExpression
    deriving (Show)

-- Relational operators

data RelationalExpression =
    ShiftRelationalExpression ShiftExpression
    | LessThanRelationalExpression RelationalExpression ShiftExpression
    | GreaterThanRelationalExpression RelationalExpression ShiftExpression
    | LessThanEqualsRelationalExpression RelationalExpression ShiftExpression
    | GreaterThanEqualsRelationalExpression RelationalExpression ShiftExpression
    | InstanceOfRelationalExpression RelationalExpression ShiftExpression
    | InRelationalExpression RelationalExpression ShiftExpression
    deriving (Show)

-- Bitwise shift operators

data ShiftExpression = 
    AdditiveShiftExpression AdditiveExpression
    | LeftShiftExpression ShiftExpression AdditiveExpression
    | RightShiftExpression ShiftExpression AdditiveExpression
    | UnsignedRightShiftExpression ShiftExpression AdditiveExpression
    deriving (Show)

-- Additive operators

data AdditiveExpression = 
    MultAdditiveExpression MultiplicativeExpression
    | PlusAdditiveExpression AdditiveExpression MultiplicativeExpression
    | MinusAdditiveExpression AdditiveExpression MultiplicativeExpression
    deriving (Show)

-- Multiplicative operators

data MultiplicativeExpression =
    UnaryMultiplicativeExpression UnaryExpression
    | MulMultiplicativeExpression MultiplicativeExpression UnaryExpression
    | DivMultiplicativeExpression MultiplicativeExpression UnaryExpression
    | ModulusMultiplicativeExpression MultiplicativeExpression UnaryExpression
    deriving (Show)

-- Unary operators

data UnaryExpression =
    PostfixUnaryExpression PostfixExpression
    | DeleteUnaryExpression UnaryExpression
    | VoidUnaryExpression UnaryExpression
    | TypeOfUnaryExpression UnaryExpression
    | IncrementPlusUnaryExpression UnaryExpression
    | IncrementMinusUnaryExpression UnaryExpression
    | PlusUnaryExpression UnaryExpression
    | MinusUnaryExpression UnaryExpression
    | BitwiseNotUnaryExpression UnaryExpression
    | LogicalNotUnaryExpression UnaryExpression
    deriving (Show)

-- Postfix expressions

data PostfixExpression =
    LHSPostfixExpression LeftHandSideExpression
    | IncrementPlusPostfixExpression LeftHandSideExpression
    | IncrementMinusPostfixExpression LeftHandSideExpression
    deriving (Show)



