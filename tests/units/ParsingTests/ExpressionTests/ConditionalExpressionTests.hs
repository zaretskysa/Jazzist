{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ParsingTests.ExpressionTests.ConditionalExpressionTests
(
    htf_thisModulesTests
) where

import Test.Framework
import Test.Framework.TestTypes
import Text.Show.Pretty

import Parsing.Parser
import ParsingTests.ParsingTestUtils


successful :: String -> ConditionalExpression -> Assertion
successful input expected = 
    case conditionalExpressionFromString input of
        (Left _) -> assertFailure "Expected successful parsing, but error happened"
        (Right actual) ->  assertEqual expected actual

failed :: String -> Assertion
failed input = 
    case conditionalExpressionFromString input of
        (Left _) -> assertBool True
        (Right result) ->  assertFailure ("Expected failed parsing, but got\n" ++ ppShow result)


-- Conditional expressions

test_unaryCondExpr = successful "i"
    (LogicalOrConditionalExpression
      (makeLogicalOrExpression "i") )

test_ternary1CondExpr = successful "i ? j : k"
    (TeranaryOperatorConditionalExpression
      (makeLogicalOrExpression "i")
      (makeAssignmentExpression "j")
      (makeAssignmentExpression "k") )

test_ternary2CondExpr = 
    successful "isGoodMood ? 'Good morning, sir.' : 'Fuck off, you scumsucker!'"
        (TeranaryOperatorConditionalExpression
          (makeLogicalOrExpression "isGoodMood")
          (makeAssignmentExpression "'Good morning, sir.'")
          (makeAssignmentExpression "'Fuck off, you scumsucker!'") )

test_nestedTernaryOperatorsCondExpr = successful "true ? false ? 2 : 3 : 4"
    (TeranaryOperatorConditionalExpression
      (makeLogicalOrExpression "true")
      (ConditionalAssignmentExpression
        (TeranaryOperatorConditionalExpression
          (makeLogicalOrExpression "false")
          (makeAssignmentExpression "2")
          (makeAssignmentExpression "3") ) )
      (makeAssignmentExpression "4") )

--TODO: combine various cases

--TODO: failing tests

--TODO: implement various NoIn tests
