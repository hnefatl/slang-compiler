{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interpreters.Interpreter0Monad
(
    Interpreter0,
    getEnvironment,
    getValue,
    setValue,
    local,
    runtimeError,
    runInterpreter0,

    liftIO -- Export liftIO from the Control.Monad.IO.Class module, as it's useful for doing IO actions in this monad
) where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M

type Environment k v = M.Map k v

newtype Interpreter0 k v e a = Interpreter0
    {
        run :: StateT (Environment k v) (ExceptT e IO) a
    }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState (Environment k v), MonadError e)

getEnvironment :: Interpreter0 k v e (Environment k v)
getEnvironment = get

getValue :: Ord k => k -> e -> Interpreter0 k v e v
getValue k e = do
                val <- gets (M.lookup k)
                case val of
                    Just v  -> return v
                    Nothing -> throwError e

setValue :: Ord k => k -> v -> Interpreter0 k v e ()
setValue k v = modify (M.insert k v)

local :: Ord k => k -> v -> Interpreter0 k v e a -> Interpreter0 k v e a
local k v a = Interpreter0 $ withStateT (M.insert k v) (run a)

runtimeError :: e -> Interpreter0 k v e a
runtimeError = throwError

runInterpreter0 :: Interpreter0 k v e a -> IO (Either e a)
runInterpreter0 a = runExceptT $ evalStateT (run a) M.empty