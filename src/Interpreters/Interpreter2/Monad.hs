{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interpreters.Interpreter2.Monad
(

) where

import Interpreters.Error

import Control.Monad.Except
import Control.Monad.State

newtype Interpreter2 k v a = Interpreter2
    {
        run :: StateT () (ExceptT Error IO) a
    }
    deriving (Functor, Applicative, Monad, MonadIO)