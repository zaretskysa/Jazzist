module Evaluating.NamedDataProperty
(
    NamedDataProperty(..)
) where

import Evaluating.Value

data NamedDataProperty = NamedDataProperty 
    { ndpName :: String
    , ndpValue :: Value
    , ndpWritable :: Bool
    , ndpEnumerable :: Bool
    , ndpConfigurable :: Bool
    } deriving (Show)
