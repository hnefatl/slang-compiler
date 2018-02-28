module Interpreters.Interpreter2.Internal.MutuallyDependent where

import Control.Monad (liftM, liftM2)
import qualified Interpreters as I (Result(..))
import Interpreters (ResultConvertible, convertResult)
import qualified Interpreters.Ast as A
import qualified Data.Map as M

-- Values and Instructions are mutually recursive data structures, so have to be defined inside the same module.
-- We can avoid having just one include for both by making this file internal, then having the "front-facing"
-- modules import this file then only export what they'd define normally.

type Address = Int
type Environment = M.Map A.VariableName Value

type ValueStack = [Value]

data Value = Unit
           | Integer Integer
           | Boolean Bool
           | Ref Address
           | Pair Value Value
           | Inl Value
           | Inr Value
           | Closure [Instruction]
            -- Check this is necessary - closures can only be recursive if they're in a let statement,
            -- and I feel like we can probably just bind the function name there and get away with it.
            -- | RecClosure A.VariableName [Instruction]
           deriving (Eq, Show)

data Instruction = Push Value
                 | Pop
                 | UnaryOp A.UOp
                 | BinaryOp A.BOp
                 | Assign
                 | Swap
                 | Bind A.VariableName
                 | Lookup A.VariableName
                 | Fst
                 | Snd
                 | Deref
                 | Apply
                 | MakePair
                 | MakeInl
                 | MakeInr
                 | MakeRef
                 | MakeLambda A.VariableName [Instruction] -- argument name, code
                 | MakeFun A.VariableName [Instruction] -- function name, code to make a lambda
                 | Test [Instruction] [Instruction]
                 | Case [Instruction] [Instruction]
                 | While [Instruction] [Instruction]
                 | Input
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
    --convertResult (RecClosure _ _) = Nothing