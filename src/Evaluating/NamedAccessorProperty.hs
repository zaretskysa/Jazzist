module Evaluating.NamedAccessorProperty
(
    NamedAccessorProperty(..)
) where

import {-# SOURCE #-} Evaluating.Object

data NamedAccessorProperty = NamedAccessorProperty
    { get :: Maybe ObjectId
    , set :: Maybe ObjectId
    , enumerable :: Bool
    , configurable :: Bool
    } deriving (Show)
