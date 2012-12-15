module Parsing.Function where

data FunctionBody sourceElement = 
    FunctionBody [sourceElement] 
    deriving (Show)
