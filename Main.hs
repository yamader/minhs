import Constraint
import Data.List
import Term
import Type

-- headSymbol

headSymbol :: Term -> String
headSymbol (Var x)    = x
headSymbol (Const f)  = f
headSymbol (App t1 _) = headSymbol t1

-- headSymbols

headSymbols :: TRS -> [String]
headSymbols r = nub [headSymbol l | (l, _) <- r]

-- main

la = TApp (TConst "L") (TVar "a")
gamma = [("c", TFun (TVar "a") (TFun la la)), ("nil", la)]
trs = [(App (App (Const "f") (App (App (Const "c") (Var "x")) (Var "y"))) (Var "z"),
        App (App (Const "c") (Var "x")) (App (App (Const "f") (Var "y")) (Var "z"))),
       (App (App (Const "f") (Const "nil")) (Var "x"), Var "x")]

main :: IO ()
main = do
  let (Just env) = mgu $ constraintForTRS gamma trs
      syms = headSymbols trs
  putStrLn.show $ [(sym, t) | sym <- syms, let (Just t) = lookup (dvar sym) env]
