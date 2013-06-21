module Evaluating.NamedAccessorProperty
(
    NamedAccessorProperty(..)
) where

import {-# SOURCE #-} Evaluating.Object

data NamedAccessorProperty = NamedAccessorProperty
    { napGet :: Maybe Object
    , napSet :: Maybe Object
    , napEnumerable :: Bool
    , napConfigurable :: Bool
    } deriving (Show)
