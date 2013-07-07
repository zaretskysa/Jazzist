module Evaluating.Property where

import Evaluating.NamedDataProperty
import Evaluating.NamedAccessorProperty

data Property = 
    DataProperty NamedDataProperty
    | AccessorProperty NamedAccessorProperty

instance Show Property
