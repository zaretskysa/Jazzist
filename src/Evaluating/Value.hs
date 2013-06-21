module Evaluating.Value
(
    Value(..),
    Primitive(..)
) where

import {-# SOURCE #-} Evaluating.Object

data Value =
    UndefinedValue
    | NullValue
    | BooleanValue Bool
    | StringValue String
    | NumberValue Double
    | ObjectValue Object
    deriving (Show)

data Primitive = 
    UndefinedPrimitive
    | NullPrimitive
    | BooleanPrimitive Bool
    | StringPrimitive String
    | NumberPrimitive Double
    deriving (Show)