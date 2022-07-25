{-# OPTIONS_GHC -Wall #-}
module Contex where 

import Syntax 

distinct :: Eq a => [a] -> Bool 
distinct []     = True 
distinct (v:vs) = notElem v vs && distinct vs 

iswfData :: String -> Static -> EnvStat -> Bool 
iswfData st sd env = maybe False (==sd)(lookup st env)

iswfProgram :: Program -> Bool
iswfProgram pr = iswfStmt pr []

iswfProc :: Proc -> EnvStat -> Bool
iswfProc (px,st) env = let psx = map (\v -> (v,VarSt)) px 
                       in distinct (map fst psx) && iswfStmt st (psx++env)    

iswfStmt :: Stmt -> EnvStat -> Bool 
iswfStmt (Assign var e) env = iswfData var VarSt env && iswfExpr e env 
iswfStmt (If e s) env       = iswfExpr e env && iswfStmt s env
iswfStmt (While e s) env    = iswfExpr e env && iswfStmt s env 
iswfStmt (Call p vs) env    = iswfData p (ProcSt (length vs)) env && all (\v -> iswfData v VarSt env) vs 
iswfStmt (Block vs ps sts) env = 
         let pts = map (\p ->(fst p, ProcSt $ length $ fst $ snd p)) ps 
             new = map (\v -> (v,VarSt)) vs ++ pts
         in  distinct (map fst new) 
           && all (((flip iswfProc (new++env) . snd) )) ps 
           && all (flip iswfStmt (new++env)) sts 
iswfStmt (Read var) env     = iswfData var VarSt env
iswfStmt (Write e) env      = iswfExpr e env

iswfExpr :: Expr -> EnvStat -> Bool 
iswfExpr (Var s) env         = iswfData s VarSt env
iswfExpr (Const _) _         = True
iswfExpr (BinOp _ e1 e2) env = iswfExpr e1 env && iswfExpr e2 env