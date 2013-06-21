module Evaluating.Eval
(
    Eval,

    runEval,

    activeContext
) where

import Control.Monad.Identity
import Control.Monad.State
import qualified Evaluating.Environment as Env
import Evaluating.ExecutionContext

type Eval a = StateT Env.Environment Identity a

runEval :: Eval a -> (a, Env.Environment)
runEval eval = runIdentity (runStateT eval Env.newEnvironment)

activeContext :: Eval ExecutionContext
activeContext = do
    env <- get
    return $ Env.activeContext env

