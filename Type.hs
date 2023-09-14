module Type where

import Term

data Type = TVar String | TConst String | TApp Type Type | TFun Type Type
  deriving Eq

type TypeEnv = [(String, Type)]

instance Show Type where
  show = showType

-- showType

showType :: Type -> String
showType (TVar a)     = a
showType (TConst c)   = c
showType (TApp t1 t2) = showType t1 ++ " " ++ showType' t2
  where showType' t@(TApp _ _) = "(" ++ showType t ++ ")"
        showType' t            = showType t
showType (TFun t1 t2) = showType t1 ++ " -> " ++ showType' t2
  where showType' t@(TFun _ _) = "(" ++ showType t ++ ")"
        showType' t            = showType t

-- typeSubstitute

typeSubstitute :: Type -> TypeEnv -> Type
typeSubstitute (TVar a) gamma
  | Just t <- lookup a gamma = t
  | otherwise                = TVar a
typeSubstitute (TConst c) _       = TConst c
typeSubstitute (TApp t1 t2) gamma = TApp (typeSubstitute t1 gamma) (typeSubstitute t2 gamma)
typeSubstitute (TFun t1 t2) gamma = TFun (typeSubstitute t1 gamma) (typeSubstitute t2 gamma)

-- compose

compose :: TypeEnv -> TypeEnv -> TypeEnv
compose gamma gamma' = [(l, typeSubstitute r gamma') | (l, r) <- gamma] ++ gamma'

-- occursAt

occursAt :: String -> Type -> Bool
occursAt a (TVar b)     = a == b
occursAt _ (TConst _)   = False
occursAt a (TApp t1 t2) = a `occursAt` t1 || a `occursAt` t2
occursAt a (TFun t1 t2) = a `occursAt` t1 || a `occursAt` t2

-- mgu

mgu :: [(Type, Type)] -> Maybe TypeEnv
mgu e = mgu' [] e

mgu' :: TypeEnv -> [(Type, Type)] -> Maybe TypeEnv
mgu' gamma [] = Just gamma

mgu' gamma ((TVar a, t):ts) = case t of
  (TVar b) | a == b    -> mgu' gamma ts
           | otherwise -> next
  _ -> next
  where next = if a `occursAt` t then Nothing else
                 let s = [(a, t)] in
                   mgu' (compose gamma s) [(typeSubstitute l s, typeSubstitute r s) | (l, r) <- ts]

mgu' gamma ((t, v@(TVar _)):ts) = mgu' gamma ((v, t):ts)
mgu' gamma ((TConst c, TConst d):ts)
  | c == d    = mgu' gamma ts
  | otherwise = Nothing
mgu' gamma ((TApp t1 t2, TApp s1 s2):ts) = mgu' gamma $ [(t1, s1), (t2, s2)] ++ ts
mgu' gamma ((TFun t1 t2, TFun s1 s2):ts) = mgu' gamma $ [(t1, s1), (t2, s2)] ++ ts
mgu' _ _ = Nothing
