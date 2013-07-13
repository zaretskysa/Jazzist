module Evaluating.ObjectsHeap
(
    module Evaluating.Object,

    ObjectsHeap,

    get,
    put,
    has,
    lookup,
    modifyObject,
) where

import Prelude hiding (lookup)
import qualified Data.Map as Map

import {-# SOURCE #-} Evaluating.Object

type ObjectsHeap = Map.Map ObjectId Object

modifyObject :: ObjectsHeap -> ObjectId -> Object -> ObjectsHeap
modifyObject heap objId newObj =
    case Map.member objId heap of
        True -> Map.insert objId newObj heap
        False -> error "There is no stored object with given id"

get :: ObjectsHeap -> ObjectId -> Object
get heap objId = case lookup heap objId of
    Just obj -> obj
    Nothing -> error $ "There is no object with id " ++ (show objId)

put :: ObjectsHeap -> ObjectId -> Object -> ObjectsHeap
put = undefined

has :: ObjectsHeap -> ObjectId -> Bool
has = undefined

lookup :: ObjectsHeap -> ObjectId -> Maybe Object
lookup heap objId = Map.lookup objId heap
