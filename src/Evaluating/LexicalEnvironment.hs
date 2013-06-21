module Evaluating.LexicalEnvironment
(
    LexicalEnvironment,

    makeLexicalEnvironment,
    getIdentifierReference
) where

import Evaluating.Reference
import Evaluating.EnvironmentRecord

data LexicalEnvironment = LexicalEnvironment
    {
        environmentRecord :: EnvironmentRecord
    }
    deriving (Show)

makeLexicalEnvironment :: LexicalEnvironment
makeLexicalEnvironment = undefined

getIdentifierReference :: Maybe LexicalEnvironment -> String -> Reference
getIdentifierReference _ _  = undefined





