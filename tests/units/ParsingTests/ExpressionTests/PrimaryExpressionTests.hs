{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.ExpressionTests.PrimaryExpressionTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes

import Parsing.Parser

successful :: String -> PrimaryExpression -> Assertion
successful input expected = 
    case primaryExpressionFromString input of
        (Left _) -> assertFailure "Expected successful parsing, but error happened"
        (Right actual) ->  assertEqual expected actual

failed :: String -> Assertion
failed input = 
    case primaryExpressionFromString input of
        (Left _) -> assertBool True
        (Right _) ->  assertFailure "Expected failed parsing"


-- Helper functions

nullAssignExpr :: AssignmentExpression
nullAssignExpr = 
    ConditionalAssignmentExpression (
      LogicalOrConditionalExpression (
        UnaryLogicalOrExpression (
          UnaryLogicalAndExpression (
            UnaryBitwiseOrExpression (
              UnaryBitwiseXorExpression (
                UnaryBitwiseAndExpression (
                  RelationalEqualityExpression (
                    ShiftRelationalExpression (
                      AdditiveShiftExpression (
                        MultAdditiveExpression (
                          UnaryMultiplicativeExpression (
                            PostfixUnaryExpression (
                              LHSPostfixExpression (
                                NewLHSExpression (
                                  MemberNewExpression (
                                    PrimaryMemberExpression (
                                      LiteralPrimaryExpression
                                        NullLiteral ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) 

literalAssignExpr :: Literal -> AssignmentExpression
literalAssignExpr literal = 
    ConditionalAssignmentExpression (
      LogicalOrConditionalExpression (
        UnaryLogicalOrExpression (
          UnaryLogicalAndExpression (
            UnaryBitwiseOrExpression (
              UnaryBitwiseXorExpression (
                UnaryBitwiseAndExpression (
                  RelationalEqualityExpression (
                    ShiftRelationalExpression (
                      AdditiveShiftExpression (
                        MultAdditiveExpression (
                          UnaryMultiplicativeExpression (
                            PostfixUnaryExpression (
                              LHSPostfixExpression (
                                NewLHSExpression (
                                  MemberNewExpression (
                                    PrimaryMemberExpression (
                                      LiteralPrimaryExpression
                                        literal ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) 

varAssignNullPropertyAssignment :: String -> PropertyAssignment
varAssignNullPropertyAssignment var = 
    FieldPropertyAssignment (StringPropertyName var) nullAssignExpr

varAssignLiteralPropertyAssignment :: String -> Literal-> PropertyAssignment
varAssignLiteralPropertyAssignment var literal = 
    FieldPropertyAssignment (StringPropertyName var) (literalAssignExpr literal)


-- Primary expressuins

test_thisPrimExpr = successful "this" ThisPrimaryExpression

test_identifierPrimExpr = successful "id" 
    (IdentifierPrimaryExpression "id")

test_literalPrimExpr = successful "null"
    (LiteralPrimaryExpression NullLiteral)

test_arrayLiteralPrimExpr = successful "[]"
    (ArrayLiteralPrimaryExpression $ ArrayLiteral [])

test_objectLiteralPrimExpr = successful "{}"
    (ObjectLiteralPrimaryExpression $ ObjectLiteral [])

test_GrouppingOperatorPrimExpr = successful "(null)"
    (ExpressionPrimaryExpression $ Expression [nullAssignExpr])


-- Literals

test_booleanLiteral = successful "true"
    (LiteralPrimaryExpression $ BooleanLiteral True)

test_numericLiteral = successful "7"
    (LiteralPrimaryExpression $ NumericLiteral 7)

test_stringLiteral = successful "'hell'"
    (LiteralPrimaryExpression $ StringLiteral "hell")

test_nullLiteral = successful "null"
    (LiteralPrimaryExpression NullLiteral)


-- Array literals

mbNullAssignExpr :: MaybeAssignmentExpression
mbNullAssignExpr = Just nullAssignExpr

test_emptyArrayLiteral = successful "[]"
    (ArrayLiteralPrimaryExpression $ ArrayLiteral [])

test_singleElisionArrayLiteral = successful "[,]"
    (ArrayLiteralPrimaryExpression $ ArrayLiteral [Nothing])

test_twoElisionsArrayLiteral = successful "[,,]"
    (ArrayLiteralPrimaryExpression $ ArrayLiteral [Nothing, Nothing])

test_threeElisionsArrayLiteral = successful "[,,,]"
    (ArrayLiteralPrimaryExpression $ ArrayLiteral [Nothing, Nothing, Nothing])

test_singleElementArrayLiteral = successful "[null]"
    (ArrayLiteralPrimaryExpression $ ArrayLiteral [mbNullAssignExpr])

test_twoElementsArrayLiteral = successful "[null, null]"
    (ArrayLiteralPrimaryExpression $ ArrayLiteral [mbNullAssignExpr, mbNullAssignExpr])

test_elementListWithEndingElisionArrayLiteral = successful "[null,]"
    (ArrayLiteralPrimaryExpression $ ArrayLiteral [mbNullAssignExpr])

test_elementListWithTwoEndingElisionsArrayLiteral = successful "[null,,]"
    (ArrayLiteralPrimaryExpression $ ArrayLiteral [mbNullAssignExpr, Nothing])

test_elementListWithStartingElisionArrayLiteral = successful "[,null]"
    (ArrayLiteralPrimaryExpression $ ArrayLiteral [Nothing, mbNullAssignExpr])

test_elementListWithTwoStartingElisionsArrayLiteral = successful "[,,null]"
    (ArrayLiteralPrimaryExpression $ ArrayLiteral [Nothing, Nothing, mbNullAssignExpr])

test_elementListWithStartingAndEndingElisionsArrayLiteral = successful "[,,null,,]"
    (ArrayLiteralPrimaryExpression $ ArrayLiteral [Nothing, Nothing, mbNullAssignExpr, Nothing])

test_elementListWithWithinElisionsArrayLiteral = successful "[,,null,,null,,]"
    (ArrayLiteralPrimaryExpression $ ArrayLiteral 
        [Nothing, Nothing, mbNullAssignExpr, Nothing, mbNullAssignExpr, Nothing])


-- Object literals

test_emptyObjectLiteral = successful "{}"
    (ObjectLiteralPrimaryExpression $ ObjectLiteral [])

test_singlePropertyAssignmentObjectLiteral = successful "{x: null}"
    (ObjectLiteralPrimaryExpression $ ObjectLiteral [varAssignNullPropertyAssignment "x"])

test_singlePropertyAssignmentWithEndingCommaObjectLiteral = 
    successful "{x: null, }"
    (ObjectLiteralPrimaryExpression $ ObjectLiteral [varAssignNullPropertyAssignment "x"])

test_singlePropertyAssignmentWithTwoEndingCommasObjectLiteral = failed "{x: null,, }"

test_singlePropertyAssignmentWithTwoColonsObjectLiteral = failed "{x:: null}"

test_fieldsPropertyAssignmentObjectLiteral = 
    successful "{name: 'Sergey', age: 24, married: false}"
    (ObjectLiteralPrimaryExpression $ ObjectLiteral 
        [ varAssignLiteralPropertyAssignment "name" (StringLiteral "Sergey")
        , varAssignLiteralPropertyAssignment "age" (NumericLiteral 24)
        , varAssignLiteralPropertyAssignment "married" (BooleanLiteral False)
        ])

test_getterPropertyAssignmentObjectLiteral = 
    successful "{get myGetter() {} }"
    (ObjectLiteralPrimaryExpression $ ObjectLiteral 
        [ GetterPropertyAssignment (StringPropertyName "myGetter") (FunctionBody []) ] )

test_getterPropertyAssignmentWithParamsObjectLiteral = failed "{get myGetter(x) {} }"

test_setterPropertyAssignmentObjectLiteral = 
    successful "{set mySetter(x) {} }"
    (ObjectLiteralPrimaryExpression $ ObjectLiteral 
        [ SetterPropertyAssignment (StringPropertyName "mySetter") ("x") (FunctionBody []) ] )

test_setterPropertyAssignmentWithoutParameterObjectLiteral = failed "{set mySetter() {} }"

test_setterPropertyAssignmentWithTwoParametersObjectLiteral = failed "{set mySetter(x, y) {} }"


-- Groupping operator

test_singleGrouppingOperator = successful "(null)"
    (ExpressionPrimaryExpression $ Expression [nullAssignExpr])

test_doubleGrouppingOperator = successful "((null))"
    (ExpressionPrimaryExpression (
      Expression
        [ ConditionalAssignmentExpression (
            LogicalOrConditionalExpression (
              UnaryLogicalOrExpression (
                UnaryLogicalAndExpression (
                  UnaryBitwiseOrExpression (
                    UnaryBitwiseXorExpression (
                      UnaryBitwiseAndExpression (
                        RelationalEqualityExpression (
                          ShiftRelationalExpression (
                            AdditiveShiftExpression (
                              MultAdditiveExpression (
                                UnaryMultiplicativeExpression (
                                  PostfixUnaryExpression (
                                    LHSPostfixExpression (
                                      NewLHSExpression (
                                        MemberNewExpression (
                                          PrimaryMemberExpression (
                                            ExpressionPrimaryExpression (
                                              Expression
                                                [ nullAssignExpr ] ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ] ) )
