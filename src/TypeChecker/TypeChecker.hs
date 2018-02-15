module TypeChecker.TypeChecker
(
    inferType,
    typecheck
) where

import qualified Parser.Types as T
import qualified Parser.Expressions as E

import Control.Monad.Trans.Reader
import Data.Map.Lazy as M

inferType :: E.Expr -> Reader (M.Map E.Variable T.Type) (Maybe T.Type)
inferType (E.SimpleExpr e) = inferTypeSimple e
inferType _ = return Nothing

inferTypeSimple :: E.SimpleExpr -> Reader (M.Map E.Variable T.Type) (Maybe T.Type)
inferTypeSimple E.Unit = return (Just T.Unit)
inferTypeSimple (E.Integer _) = return (Just T.Integer)
inferTypeSimple (E.Boolean _) = return (Just T.Boolean)
inferTypeSimple (E.Identifier name) = reader (M.lookup name)
inferTypeSimple (E.Deref e) = do
                                innerMaybeType <- inferTypeSimple e
                                return (fmap T.Ref innerMaybeType)
inferTypeSimple (E.Pair l r) = do
                                lType <- inferType l
                                rType <- inferType r
                                return (lType >>= T.Product lType rType)

inferTypeSimple (E.Expr e) = inferType e



typecheck :: E.Expr -> Maybe String
typecheck = undefined