module Evaluating.LexicalEnvM
(
    module Evaluating.Eval,
    module Evaluating.Reference,

    getIdentifierReference,
) where

import Evaluating.Eval
import Evaluating.Reference
import qualified Evaluating.LexicalEnvironment as LexEnv

getIdentifierReference :: String -> Eval Reference
getIdentifierReference identifier = do
    lexEnv <- activeLexEnv
    return $ LexEnv.getIdentifierReference lexEnv identifier




