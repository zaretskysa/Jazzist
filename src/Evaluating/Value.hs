module Evaluating.Value
(
    Value(..)
) where

data Value =
    UndefinedValue
    | NullValue
    | BooleanValue Bool
    | StringValue String
    | NumberValue Double
    | ObjectValue
    deriving (Show)
