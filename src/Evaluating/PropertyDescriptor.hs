module Evaluating.PropertyDescriptor
(
    PropertyDescriptor(..),
    MaybePropertyDescriptor,

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
    bothNotConfigurable,
    isGenericOrData,
    onlyOneIsData,
    onlyOneIsEnumerable,
    bothAreData,
    bothAreAccessors,
    isWritable,
    isNotWritable,
    hasValue,
    haveDifferentValues,
    haveDifferentSetters,
    hasGetter,
    haveDifferentGetters,
) where

import Data.Maybe

import Common.BoolUtils
import Evaluating.Value
import {-# SOURCE #-} Evaluating.Object


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

isGenericOrData :: PropertyDescriptor -> Bool
isGenericOrData desc = (isGeneric desc) || (isData desc)

isGenericMb :: MaybePropertyDescriptor -> Bool
isGenericMb Nothing = False
isGenericMb (Just desc) = isGeneric desc

new :: PropertyDescriptor
new = PropertyDescriptor {
    get = Nothing,
    set = Nothing,
    value = Nothing,
    writable = Nothing,
    enumerable = Nothing,
    configurable = Nothing
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

bothNotConfigurable :: PropertyDescriptor -> PropertyDescriptor -> Bool
bothNotConfigurable first second =
    isNotConfigurable first && isNotConfigurable second

onlyOneIsData :: PropertyDescriptor -> PropertyDescriptor -> Bool
onlyOneIsData first second = xor (isData first) (isData second)

onlyOneIsEnumerable :: PropertyDescriptor -> PropertyDescriptor -> Bool
onlyOneIsEnumerable first second = xor (isEnumerable first) (isEnumerable second)

bothAreData :: PropertyDescriptor -> PropertyDescriptor -> Bool
bothAreData first second = (isData first) && (isData second)

bothAreAccessors :: PropertyDescriptor -> PropertyDescriptor -> Bool
bothAreAccessors first second = (isAccessor first) && (isAccessor second)

isWritable :: PropertyDescriptor -> Bool
isWritable desc = maybe False id $ writable desc

isNotWritable :: PropertyDescriptor -> Bool
isNotWritable = not . isWritable

hasValue :: PropertyDescriptor -> Bool
hasValue = isJust . writable

haveDifferentValues :: PropertyDescriptor -> PropertyDescriptor -> Bool
haveDifferentValues first second = undefined

haveDifferentSetters :: PropertyDescriptor -> PropertyDescriptor -> Bool
haveDifferentSetters = undefined

hasGetter :: PropertyDescriptor -> Bool
hasGetter = undefined

haveDifferentGetters :: PropertyDescriptor -> PropertyDescriptor -> Bool
haveDifferentGetters first second = undefined
