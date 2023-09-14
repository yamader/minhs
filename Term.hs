module Term where

import Control.Applicative ((<|>))
import Data.List

data Term = Var String | Const String | App Term Term
  deriving Eq

type Rule = (Term, Term)
type TRS  = [Rule]

type Subst = [(String, Term)]

instance Show Term where
  show = showTerm

-- number

number :: Term -> Maybe Int
number (Const "0")         = Just 0
number (App (Const "s") t) = (+1) <$> number t
number _                   = Nothing

-- list

list :: Term -> Maybe [Term]
list (Const "nil")                  = Just []
list (App (App (Const "cons") s) t) = (s:) <$> list t
list _                              = Nothing

-- showTerm

showTerm :: Term -> String
showTerm t
  | Just n <- number t = show n
  | Just a <- list t   = "[" ++ intercalate ", " [showTerm i | i <- a] ++ "]"
showTerm (Var x)   = x
showTerm (Const f) = f
showTerm (App s t) = showTerm s ++ " " ++ showTerm' t

showTerm' :: Term -> String
showTerm' t@(App _ _) = "(" ++ showTerm t ++ ")"
showTerm' t           = showTerm t

-- substitute

substitute :: Term -> Subst -> Term
substitute (Var x) sigma
  | Just t <- lookup x sigma = t
  | otherwise                = Var x
substitute (Const f) _       = Const f
substitute (App t1 t2) sigma = App (substitute t1 sigma) (substitute t2 sigma)

-- match

match :: Term -> Term -> Maybe Subst
match l t = match' [] [(l, t)]

match' :: Subst -> TRS -> Maybe Subst
match' l [] = Just l

match' l ((Var x, t):ts) = case lookup x l of
  Just t' | t == t'   -> match' l ts
          | otherwise -> Nothing
  _ -> match' ((x, t):l) ts

match' l ((Const f, Const g):ts)
  | f == g    = match' l ts
  | otherwise = Nothing

match' l ((App s1 s2, App t1 t2):ts) =
  match' l $ [(s1, t1), (s2, t2)] ++ ts

match' _ _ = Nothing

-- rewriteAtRoot

rewriteAtRoot :: TRS -> Term -> Maybe Term
rewriteAtRoot [] t = Nothing
rewriteAtRoot ((l, r):rs) t
  | Just s <- match l t = Just (substitute r s)
  | otherwise           = rewriteAtRoot rs t

-- rewrite

rewrite :: TRS -> Term -> Maybe Term
rewrite r t
  | Just t' <- rewriteAtRoot r t = rewrite r t' <|> Just t'
  | otherwise                    = Nothing

-- nf

nf :: TRS -> Term -> Term
nf r t = case nf' r t of
  s | s == t    -> t
    | otherwise -> nf r s

nf' :: TRS -> Term -> Term
nf' r t = case rewrite r t <|> Just t of
  Just (App t1 t2) -> App (nf r t1) (nf r t2)
  Just t           -> t
