module Interpreters.Interpreter0
(
    interpret
) where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import qualified Data.Map as M

import qualified Interpreters.Ast as A

data Value = Integer Integer | Boolean Bool | Unit

type Error = String
type Interpreter = ExceptT Error (Reader (M.Map A.Variable Value))

interpret :: A.Ast -> Interpreter Value
interpret = undefined