module Evaluating.NamedAccessorProperty
(
    NamedAccessorProperty(..),

    toDescriptor,
) where

import {-# SOURCE #-} Evaluating.Object
import qualified Evaluating.PropertyDescriptor as PDesc


data NamedAccessorProperty = NamedAccessorProperty
    { get :: Maybe ObjectId
    , set :: Maybe ObjectId
    , enumerable :: Bool
    , configurable :: Bool
    } deriving (Show)

toDescriptor :: NamedAccessorProperty -> PDesc.PropertyDescriptor
toDescriptor prop = PDesc.PropertyDescriptor 
    { PDesc.get = get prop
    , PDesc.set = set prop
    , PDesc.enumerable = Just $ enumerable prop
    , PDesc.configurable = Just $ configurable prop
    }
