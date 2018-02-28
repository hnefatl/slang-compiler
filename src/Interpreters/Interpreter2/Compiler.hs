module Interpreters.Interpreter2.Compiler
(
    compile
) where

import Data.List (intersperse)

import qualified Interpreters.Ast as A
import Interpreters.Interpreter2.Instructions
import Interpreters.Interpreter2.Values

compile :: (A.Ast s) -> [Instruction]
compile (A.Unit _) = [Push Unit]
compile (A.Integer _ i) = [Push $ Integer i]
compile (A.Boolean _ b) = [Push $ Boolean b]
compile (A.Variable _ n) = [Lookup n]
compile (A.Deref _ e) = compile e ++ [Deref]
compile (A.Ref _ e) = compile e ++ [MakeRef]
compile (A.Pair _ l r) = compile l ++ compile r ++ [MakePair]
compile (A.UnaryOp _ op e) = compile e ++ [UnaryOp op]
compile (A.BinaryOp _ op l r) = compile l ++ compile r ++ [BinaryOp op]
compile (A.Sequence _ es) = concat $ intersperse [Pop] $ map compile es -- Pop to remove the middle results
compile (A.If _ c l r) = compile c ++ [Test (compile l) (compile r)]
compile (A.Inl _ e) = compile e ++ [MakeInl]
compile (A.Inr _ e) = compile e ++ [MakeInr]
compile (A.Case _ e l r) = compile e ++ [Case (compile l) (compile r)]
compile (A.Fst _ e) = compile e ++[Fst]
compile (A.Snd _ e) = compile e ++ [Snd]
compile (A.While _ c e) = [While (compile c) (compile e)]
compile (A.Let _ n b e) = compile b ++ [Bind n] ++ compile e
compile (A.LetFun _ n f e) = [MakeFun n (compile f)] ++ [Bind n] ++ compile e
compile (A.Fun _ x e) = [MakeLambda x (compile e)]
compile (A.Application _ f x) = compile x ++ compile f ++ [Apply] -- Arg before function, for compat with Tim's compiler
compile (A.Input _) = [Input]