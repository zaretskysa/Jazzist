module Parsing.Data.Expression where

data Expression = 
    Expression [AssignmentExpression] -- TODO: non empty
    deriving (Show)

data AssignmentExpression = 
    AssignmentExpression deriving (Show) -- TODO: remove this stub
