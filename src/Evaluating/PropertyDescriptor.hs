module Evaluating.PropertyDescriptor
(
    PropertyDescriptor(..),
    MaybePropertyDescriptor,

    fromProperty,

    isAccessor,
    isData,
    isGeneric,

    get,
    hasSetter,
    set,
    new,
    value,
    writable,
    configurable,
    enumerable,
    allFieldsAreAbsent,
    isSame,
    isConfigurable,
    isNotConfigurable,
    hasEnumerable,
    isEnumerable,
) where

import Data.Maybe

import Evaluating.Value
import {-# SOURCE #-} Evaluating.Object
import {-# SOURCE #-} Evaluating.Property
import qualified Evaluating.NamedDataProperty as NProp
import Evaluating.NamedDataProperty (NamedDataProperty)
import qualified Evaluating.NamedAccessorProperty as AProp
import Evaluating.NamedAccessorProperty (NamedAccessorProperty)

type MaybePropertyDescriptor = Maybe PropertyDescriptor

data PropertyDescriptor = PropertyDescriptor 
    { get :: Maybe ObjectId --rename to setter
    , set :: Maybe ObjectId --rename to getter
    , value :: Maybe Value 
    , writable :: Maybe Bool
    , enumerable :: Maybe Bool
    , configurable :: Maybe Bool -- does we need Maybe ??
    } deriving (Show)

isAccessor :: PropertyDescriptor -> Bool
isAccessor desc 
    | isNothing $ get desc, isNothing $ set desc = False
    | otherwise = True

isAccessorMb :: MaybePropertyDescriptor -> Bool
isAccessorMb Nothing = False
isAccessorMb (Just desc) = isAccessor desc

isData :: PropertyDescriptor -> Bool
isData desc 
    | isNothing $ value desc, isNothing $ writable desc = False
    | otherwise = True

isDataMb :: MaybePropertyDescriptor -> Bool
isDataMb Nothing = False
isDataMb (Just desc) = isData desc

isGeneric :: PropertyDescriptor -> Bool
isGeneric desc
    | not $ isAccessor desc, not $ isData desc = True
    | otherwise = False

isGenericMb :: MaybePropertyDescriptor -> Bool
isGenericMb Nothing = False
isGenericMb (Just desc) = isGeneric desc

fromPropertyDescriptor :: PropertyDescriptor -> Object
fromPropertyDescriptor desc = undefined

toPropertyDescriptor :: Object -> PropertyDescriptor
toPropertyDescriptor obj = undefined

new :: PropertyDescriptor
new = PropertyDescriptor {
    get = Nothing,
    set = Nothing,
    value = Nothing,
    writable = Nothing,
    enumerable = Nothing,
    configurable = Nothing
    }

fromProperty :: Property -> PropertyDescriptor
fromProperty (DataProperty prop) = fromDataProperty prop
fromProperty (AccessorProperty prop) = fromAccessorProperty prop

fromDataProperty :: NamedDataProperty -> PropertyDescriptor
fromDataProperty dataProp = PropertyDescriptor 
    { value = Just $ NProp.value dataProp
    , writable = Just $ NProp.writable dataProp
    , enumerable = Just $ NProp.enumerable dataProp
    , configurable = Just $ NProp.configurable dataProp
    }

fromAccessorProperty :: NamedAccessorProperty -> PropertyDescriptor
fromAccessorProperty accessorProp = PropertyDescriptor 
    { get = AProp.get accessorProp
    , set = AProp.set accessorProp
    , enumerable = Just $ AProp.enumerable accessorProp
    , configurable = Just $ AProp.configurable accessorProp
    }

hasSetter :: PropertyDescriptor -> Bool
hasSetter = isJust . set

allFieldsAreAbsent :: PropertyDescriptor -> Bool
allFieldsAreAbsent desc =
    (isNothing $ get desc)  &&
    (isNothing $ set desc)  &&
    (isNothing $ value desc)  &&
    (isNothing $ writable desc)  &&
    (isNothing $ enumerable desc)  &&
    (isNothing $ configurable desc)

isSame :: PropertyDescriptor -> PropertyDescriptor -> Bool
isSame = undefined

isConfigurable :: PropertyDescriptor -> Bool
isConfigurable = undefined

isNotConfigurable :: PropertyDescriptor -> Bool
isNotConfigurable = not . isConfigurable

hasEnumerable :: PropertyDescriptor -> Bool
hasEnumerable = isJust . enumerable

isEnumerable :: PropertyDescriptor -> Bool
isEnumerable = undefined
