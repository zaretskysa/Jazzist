{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.ExpressionTests.LeftHandSideExpressionTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils


successful :: String -> LeftHandSideExpression -> Assertion
successful input expected = 
    case leftHandSideExpressionFromString input of
        (Left _) -> assertFailure "Expected successful parsing, but error happened"
        (Right actual) ->  assertEqual expected actual

failed :: String -> Assertion
failed input = 
    case leftHandSideExpressionFromString input of
        (Left _) -> assertBool True
        (Right result) ->  assertFailure ("Expected failed parsing, but got\n" ++ ppShow result)


-- Member expressions

test_primaryMemberExpression = successful input (
    NewLHSExpression (
      MemberNewExpression (
        PrimaryMemberExpression (
          makePrimaryExpression input) ) ) )
    where input = "null"

test_functionMemberExpression = successful input (
    NewLHSExpression (
      MemberNewExpression (
        FunctionMemberExpression ( 
          makeFunctionExpression input ) ) ) )
    where input = "function foo(x, y) {}"

test_propertyAccessByBracketsMemberExpression = successful "x[foo]" (
    NewLHSExpression (
      MemberNewExpression (
        PropertyAccessByBracketsMemberExpression (
           PrimaryMemberExpression (
              makePrimaryExpression "x" ) )
           (makeExpression "foo" ) ) ) )

test_propertyAccessByDotMemberExpression = successful "x.foo" (
    NewLHSExpression (
      MemberNewExpression (
        PropertyAccessByDotMemberExpression (
           PrimaryMemberExpression (
              makePrimaryExpression "x" ) )
           "foo" ) ) )

test_newMemberExpression = successful "new Foo()" (
    NewLHSExpression (
      MemberNewExpression (
        NewMemberExpression (
           PrimaryMemberExpression (
              makePrimaryExpression "Foo" ) )
           [] ) ) )


-- New expressions

test_memberNewExpression = successful input (
    NewLHSExpression (
      MemberNewExpression (
        PrimaryMemberExpression (
          makePrimaryExpression input) ) ) )
    where input = "null"

test_newNewExpression = successful "new Foo" (
    NewLHSExpression (
      NewNewExpression (
        MemberNewExpression (
          PrimaryMemberExpression (
            makePrimaryExpression "Foo") ) ) ) )

test_doubleNewNewExpression = successful "new new Foo" (
    NewLHSExpression (
      NewNewExpression (
        NewNewExpression (
          MemberNewExpression (
            PrimaryMemberExpression (
              makePrimaryExpression "Foo") ) ) ) ) )


-- Call expressions

test_memberWithArgsCallExpression = successful "foo()" (
    CallLHSExpression (
      MemberWithArgumentsCallExpression (
        PrimaryMemberExpression (
          makePrimaryExpression "foo") ) 
        [] ) )

test_callWithArgsCallExpression = successful "foo()()" (
    CallLHSExpression (
      CallWithArgumentsCallExpression (
        MemberWithArgumentsCallExpression (
          PrimaryMemberExpression (
            makePrimaryExpression "foo") ) 
          [] ) 
      [] ) )

test_doubleCallWithArgsCallExpression = successful "foo()()()" (
    CallLHSExpression (
      CallWithArgumentsCallExpression (
        CallWithArgumentsCallExpression (
          MemberWithArgumentsCallExpression (
            PrimaryMemberExpression (
              makePrimaryExpression "foo") ) 
            [] ) 
        [] ) 
      [] ) )


-- Arguments

test_emptyArguments = successful "foo()" (
    CallLHSExpression (
      MemberWithArgumentsCallExpression (
        PrimaryMemberExpression (
          makePrimaryExpression "foo") ) 
        [] ) )

test_wrappedEmptyArguments = failed "foo(())"

test_singleCommaArguments = failed "foo(,)"

test_singleElementArguments = successful "foo(1)" (
    CallLHSExpression (
      MemberWithArgumentsCallExpression (
        PrimaryMemberExpression (
          makePrimaryExpression "foo") ) 
        [makeAssignmentExpression "1"] ) )

test_singleElementWithEndingCommaArguments = failed "foo(1,)"

test_twoArguments = successful "foo(1, 'hello')" (
    CallLHSExpression (
      MemberWithArgumentsCallExpression (
        PrimaryMemberExpression (
          makePrimaryExpression "foo") ) 
        [makeAssignmentExpression "1", makeAssignmentExpression "'hello'"] ) )


-- Lef hand side expressions

test_newLeftHandSideExpression = successful input (
    NewLHSExpression (
      MemberNewExpression (
        PrimaryMemberExpression (
          makePrimaryExpression input) ) ) )
    where input = "null"

test_callLeftHandSideExpression = successful "foo()" (
    CallLHSExpression (
      MemberWithArgumentsCallExpression (
        PrimaryMemberExpression (
          makePrimaryExpression "foo") )
        [] ) )
