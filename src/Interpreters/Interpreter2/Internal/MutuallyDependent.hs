module Interpreters.Interpreter2.Internal.MutuallyDependent where

import Control.Monad (liftM, liftM2)
import qualified Interpreters as I (Result(..))
import Interpreters (ResultConvertible, convertResult)
import qualified Interpreters.Ast as A

-- Values and Instructions are mutually recursive data structures, so have to be defined inside the same module.
-- We can avoid having just one include for both by making this file internal, then having the "front-facing"
-- modules import this file then only export what they'd define normally.

type Address = Int

data Value = Unit
           | Integer Integer
           | Boolean Bool
           | Ref Address
           | Pair Value Value
           | Inl Value
           | Inr Value
           | Closure [Instruction]
           | RecClosure A.VariableName [Instruction]
           deriving (Eq, Show)

data Instruction = Push Value
                 | Lookup A.VariableName
                 | UnaryOp A.UOp
                 | BinaryOp A.BOp
                 | Assign
                 | Swap
                 | Pop
                 | Bind A.VariableName
                 | Fst
                 | Snd
                 | Deref
                 | Apply
                 | MakePair
                 | MakeInl
                 | MakeInr
                 | MakeRef
                 | MakeClosure [Instruction]
                 | MakeRecClosure A.VariableName [Instruction]
                 | Test [Instruction] [Instruction]
                 | Case [Instruction] [Instruction]
                 | While [Instruction] [Instruction]
    deriving (Eq, Show)

instance ResultConvertible Value where
    convertResult Unit        = Just $ I.Unit
    convertResult (Integer i) = Just $ I.Integer i
    convertResult (Boolean b) = Just $ I.Boolean b
    convertResult (Ref _)     = Nothing
    convertResult (Pair l r)  = liftM2 I.Pair (convertResult l) (convertResult r)
    convertResult (Inl v)     = liftM I.Inl (convertResult v)
    convertResult (Inr v)     = liftM I.Inr (convertResult v)
    convertResult (Closure _) = Nothing
    convertResult (RecClosure _ _) = Nothing