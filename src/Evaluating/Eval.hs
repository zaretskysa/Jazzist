module Evaluating.Eval
(
    Eval,

    runEval
) where

import Control.Monad.Identity

type Eval a = Identity a

runEval :: Eval a -> a
runEval eval = runIdentity eval
