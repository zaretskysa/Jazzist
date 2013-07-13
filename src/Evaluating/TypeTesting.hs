module Evaluating.TypeTesting
(
    isCallable,
    isCallableMb,
    isPrimitive,
) where

import Data.Maybe

import Evaluating.Value
import qualified Evaluating.Object as Obj

isCallable :: Value -> Bool
isCallable (ObjectValue obj) = Obj.isCallable obj
isCallable _ = False

isCallableMb :: MaybeValue -> Bool
isCallableMb value = maybe False isCallable value

isPrimitive :: Value -> Bool
isPrimitive = undefined
