{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeChecker.TypeCheckerMonad
(
    TypeChecker,
    inModifiedEnv,
    fromEnv,
    typeError,
    runTypeChecker
) where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Map.Lazy as M

type Environment k v = M.Map k v

newtype TypeChecker k v e a = TypeChecker
    {
        run :: ReaderT (Environment k v) (Except e) a
    }
    deriving (Functor, Applicative, Monad, MonadReader (Environment k v), MonadError e)

inModifiedEnv :: (Environment k v -> Environment k v) -> TypeChecker k v e a -> TypeChecker k v e a
inModifiedEnv = local

fromEnv :: (Environment k v -> a) -> TypeChecker k v e a
fromEnv = asks

typeError :: e -> TypeChecker k v e a
typeError = throwError

runTypeChecker :: TypeChecker k v e a -> Either e a
runTypeChecker t = runExcept $ runReaderT (run t) M.empty