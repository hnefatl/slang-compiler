{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interpreters.Interpreter0Monad
(
    Interpreter0,
    getEnvironment,
    getValue,
    getValueE,
    setValue,
    removeValue,
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

getValue :: Ord k => k -> Interpreter0 k v e (Maybe v)
getValue k = gets (M.lookup k)

getValueE :: Ord k => k -> e -> Interpreter0 k v e v
getValueE k e = do
                    val <- getValue k
                    case val of
                        Just v  -> return v
                        Nothing -> throwError e

setValue :: Ord k => k -> v -> Interpreter0 k v e ()
setValue k v = modify (M.insert k v)

removeValue :: Ord k => k -> Interpreter0 k v e ()
removeValue k = modify (M.delete k)

local :: Ord k => k -> v -> Interpreter0 k v e a -> Interpreter0 k v e a
local k v a = do
                existingValue <- getValue k
                setValue k v
                res <- a
                case existingValue of
                    Just oldV -> setValue k oldV
                    Nothing   -> removeValue k
                return res

runtimeError :: e -> Interpreter0 k v e a
runtimeError = throwError

runInterpreter0 :: Interpreter0 k v e a -> IO (Either e a)
runInterpreter0 a = runExceptT $ evalStateT (run a) M.empty