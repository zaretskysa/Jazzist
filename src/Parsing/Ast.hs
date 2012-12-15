module Parsing.Ast where

import Parsing.Statement
import Parsing.Function
import Parsing.Expression

data Program = 
    Program [SourceElement] deriving (Show)

data SourceElement = 
    StatementSourceElement (Statement (AssignmentExpression SourceElement) (LeftHandSideExpression SourceElement)  (Expression SourceElement) SourceElement)
    | FunctionDeclarationSourceElement String [String] (FunctionBody SourceElement) -- TODO: Introduce FunctionDeclaration structure
    deriving (Show)




