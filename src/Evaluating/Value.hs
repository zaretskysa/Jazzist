module Evaluating.Value
(
    Value(..),
    MaybeValue,
    Primitive(..),
) where

import {-# SOURCE #-} Evaluating.Object

type MaybeValue = Maybe Value

data Value =
    UndefinedValue
    | NullValue
    | BooleanValue Bool
    | StringValue String
    | NumberValue Double
    | ObjectValue Object
    | ObjectIdValue ObjectId
    deriving (Show)

data Primitive = 
    UndefinedPrimitive
    | NullPrimitive
    | BooleanPrimitive Bool
    | StringPrimitive String
    | NumberPrimitive Double
    deriving (Show)
