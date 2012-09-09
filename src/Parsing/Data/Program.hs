module Parsing.Data.Program where

import Parsing.Data.Statement

data Program = 
    Program [SourceElement] deriving (Show)

data SourceElement = 
    StatementSourceElement Statement
--  | FunctionDeclarationSourceElement
    deriving (Show)
