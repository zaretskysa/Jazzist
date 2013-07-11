module Evaluating.NamedDataProperty
(
    NamedDataProperty(..),

    toDescriptor,
) where

import Evaluating.Value

import qualified Evaluating.PropertyDescriptor as PDesc

data NamedDataProperty = NamedDataProperty 
    { value :: Value
    , writable :: Bool
    , enumerable :: Bool
    , configurable :: Bool
    } deriving (Show)

toDescriptor :: NamedDataProperty -> PDesc.PropertyDescriptor
toDescriptor prop = PDesc.PropertyDescriptor
    { PDesc.value = Just $ value prop
    , PDesc.writable = Just $ writable prop
    , PDesc.enumerable = Just $ enumerable prop
    , PDesc.configurable = Just $ configurable prop
    }

fromDescriptor :: PDesc.PropertyDescriptor -> NamedDataProperty
fromDescriptor desc = NamedDataProperty
    { value = maybe UndefinedValue id $ PDesc.value desc
    , writable = PDesc.isWritable desc
    , enumerable = PDesc.isEnumerable desc
    , configurable = PDesc.isConfigurable desc
    }

