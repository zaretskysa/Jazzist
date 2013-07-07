module Evaluating.Property
(
    Property(..),
    MaybeProperty(..),
    Properties,

    dataPropertyFromDescriptor,
    accessorPropertyFromDescriptor,
) where

import qualified Data.Map as Map

import Evaluating.NamedDataProperty
import Evaluating.NamedAccessorProperty
import Evaluating.PropertyDescriptor (PropertyDescriptor)

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
