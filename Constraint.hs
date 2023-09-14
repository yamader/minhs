module Constraint where

import Term
import Type

-- rename

rename :: Term -> Term
rename (Var x)     = Var (x ++ "'")
rename (Const f)   = Const f
rename (App t1 t2) = App (rename t1) (rename t2)

-- renameTRS

renameTRS :: TRS -> TRS
renameTRS []          = []
renameTRS ((l, r):rs) = (rename l, rename r):renameTRS rs

-- temp, dvar

temp = ("a" ++) . show
dvar = ("d" ++)

-- renameType

renameType :: Type -> Int -> Type
renameType (TVar a) k     = TVar $ a ++ show k
renameType (TConst f) _     = TConst f
renameType (TApp t1 t2) k = TApp (renameType t1 k) (renameType t2 k)
renameType (TFun t1 t2) k = TFun (renameType t1 k) (renameType t2 k)

-- constraintForTerm

constraintForTerm :: TypeEnv -> Term -> Int -> ([(Type, Type)], Int)
constraintForTerm gamma (Var x) k = ([(TVar $ temp k, TVar $ dvar x)], k + 1)
constraintForTerm gamma (Const f) k
  | Just t <- lookup f gamma = ([(TVar $ temp k, renameType t (k + 1))], k + 2)
  | otherwise                = ([(TVar $ temp k, TVar $ dvar f)], k + 1)
constraintForTerm gamma (App t1 t2) k = (e:(cl ++ cr), mr)
  where (cl, ml) = constraintForTerm gamma t1 (k + 1)
        (cr, mr) = constraintForTerm gamma t2 ml
        e = (TVar $ temp (k + 1), TFun (TVar $ temp ml) (TVar $ temp k))

-- constraintForTRS

constraintForTRS :: TypeEnv -> TRS -> [(Type, Type)]
constraintForTRS gamma r = constraintForTRS' gamma r 0

constraintForTRS' :: TypeEnv -> TRS -> Int -> [(Type, Type)]
constraintForTRS' _ [] _ = []
constraintForTRS' gamma ((l, r):rs) k = c ++ constraintForTRS' gamma (renameTRS rs) mr
  where (cl, ml) = constraintForTerm gamma l k
        (cr, mr) = constraintForTerm gamma r ml
        c = cl ++ cr ++ [(TVar $ temp k, TVar $ temp ml)]
