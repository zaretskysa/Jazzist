module Evaluating.ObjectsHeap
(
    module Data.Map,
    module Evaluating.Object,

    ObjectsHeap,

    modifyObject,
) where

import Data.Map

import {-# SOURCE #-} Evaluating.Object

type ObjectsHeap = Map ObjectId Object

modifyObject :: ObjectsHeap -> ObjectId -> Object -> ObjectsHeap
modifyObject heap objId newObj =
    case member objId heap of
        True -> insert objId newObj heap
        False -> error "There is no stored object with given id"
