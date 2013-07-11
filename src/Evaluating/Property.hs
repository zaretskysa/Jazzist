module Evaluating.Property
(
    Property(..),
    MaybeProperty(..),
    Properties,

    dataPropertyFromDescriptor,
    accessorPropertyFromDescriptor,
    fromDescriptor,
    convertToAccessor,
    convertToData,
    toDescriptor,
) where

import qualified Data.Map as Map

import Evaluating.Value

import Evaluating.NamedDataProperty (NamedDataProperty(..))
import qualified Evaluating.NamedDataProperty as DProp

import Evaluating.NamedAccessorProperty (NamedAccessorProperty(..))
import qualified Evaluating.NamedAccessorProperty as AProp

import Evaluating.PropertyDescriptor (PropertyDescriptor)
import qualified Evaluating.PropertyDescriptor as PDesc

-- move to separate module
type Properties = Map.Map String Property

type MaybeProperty = Maybe Property

data Property = 
    DataProperty NamedDataProperty
    | AccessorProperty NamedAccessorProperty
    deriving (Show)

dataPropertyFromDescriptor :: PropertyDescriptor -> Property
dataPropertyFromDescriptor desc = undefined

accessorPropertyFromDescriptor :: PropertyDescriptor -> Property
accessorPropertyFromDescriptor = undefined

fromDescriptor :: PropertyDescriptor -> Property
fromDescriptor = undefined

convertToAccessor :: Property -> Property
convertToAccessor prop@(AccessorProperty _) = prop
convertToAccessor (DataProperty prop) = AccessorProperty $ NamedAccessorProperty
    { AProp.configurable = DProp.configurable prop
    , AProp.enumerable = DProp.enumerable prop
    , AProp.get = Nothing
    , AProp.set = Nothing    
    }

convertToData :: Property -> Property
convertToData prop@(DataProperty _) = prop
convertToData (AccessorProperty prop) = DataProperty $ NamedDataProperty
    { DProp.configurable = AProp.configurable prop
    , DProp.enumerable = AProp.enumerable prop
    , DProp.value = UndefinedValue
    , DProp.writable = False
    }

toDescriptor :: Property -> PropertyDescriptor
toDescriptor (DataProperty prop) = DProp.toDescriptor prop
toDescriptor (AccessorProperty prop) = AProp.toDescriptor prop
