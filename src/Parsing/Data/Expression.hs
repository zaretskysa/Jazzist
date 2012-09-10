module Parsing.Data.Expression where

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
    PrimaryMemberExpression PrimaryExpression
    | FunctionMenberExpression FunctionExpression
    | AccessByIndexMemberExpression MemberExpression Expression
    | PropertyAccessMemberExpression MemberExpression String
    | NewMemberExpression MemberExpression [AssignmentExpression]
    deriving (Show)

data FunctionExpression = FunctionExpression deriving (Show) -- stub

data NewExpression =
    MemberNewExpression MemberExpression
    | NewNewExpression NewExpression
    deriving (Show)

data CallExpression =
    MemberWithArgumentsCallExpression MemberExpression [AssignmentExpression]
    | CallWithArgumentsCallExpression CallExpression [AssignmentExpression]
    | CallWithIndexAccessCallExpression CallExpression Expression
    | CallWithPropertyAccessCallExpression CallExpression String
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
    | ArrayLiteralPrimaryExpression [MaybeAssignmentExpression]
    | ObjectLiteralPrimaryExpression [PropertyAssignment]
    | ExpressionPrimaryExpression Expression
    deriving (Show)

data Literal = 
    NullLiteral
    | BooleanLiteral Bool
    | NumericLiteral Double
    | StringLiteral String
--  | RegularExpressionLiteral TODO
    deriving (Show)

data PropertyAssignment = 
    FieldPropertyAssignment PropertyName AssignmentExpression
    | GetterPropertyAssignment FunctionBody
    | SetterPropertyAssignment PropertyName PropertySetParameterList FunctionBody
    deriving (Show)

type PropertySetParameterList = String

data PropertyName = 
    StringPropertyName String
    | NumericPropertyName Double
    deriving (Show)

data FunctionBody = FunctionBody deriving (Show)

-- Logical Expressions

data LogicalAndExpression = 
    BitwiseOrLogicalAndExpression BitwiseOrExpression
    | ConjunctionLogicalAndExpression LogicalAndExpression BitwiseOrExpression
    deriving (Show)

data LogicalOrExpression = 
    LogicalAndLogicalOrExpression LogicalAndExpression
    | DisjunctionLogicalOrExpression LogicalOrExpression LogicalAndExpression
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


