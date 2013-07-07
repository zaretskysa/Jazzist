module Evaluating.NamedDataProperty
(
    NamedDataProperty(..)
) where

import Evaluating.Value

data NamedDataProperty = NamedDataProperty 
    { name :: String
    , value :: Value
    , writable :: Bool
    , enumerable :: Bool
    , configurable :: Bool
    } deriving (Show)
