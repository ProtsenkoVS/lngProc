{-# OPTIONS_GHC -Wall #-}
module Semantics where 
-- Помилка фіксується зразу (в тому числі в базових). 
-- Працюємо повністю з error -- переривання в чистому коді
import Data.Maybe
import Syntax 
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class(lift)
import Control.Monad

data Denote = Location Int 
            | Procedure ([Denote] -> Work -> Work)

type Env = [(String,Denote)]

getDenote :: String -> Env -> Denote 
getDenote s env = fromJust $ lookup s env 

getProcedure :: String -> Env -> ([Denote] -> Work -> Work)   
getProcedure s env = case fromJust $ lookup s env of 
                   Location  _ -> error "getProcedure"        
                   Procedure f -> f                           

getLocation :: String -> Env -> Int                     
getLocation s env = case fromJust $ lookup s env of 
                   Location  k -> k                      
                   Procedure _ -> error "getLocation"   

getValue:: Int -> Work -> Integer
getValue k = \(_,stg,_) -> 
    let p = length stg - k
    in case stg!!p of
         Just v  -> v 
         Nothing -> error "valueNothing"

updValue :: Int ->  Integer -> Work -> Work
updValue k v  = \(inp,stg,out) ->
     let p = length stg - k
         (beg,end) = splitAt p stg 
         stg1 = beg ++ [Just v] ++ (tail end)
     in  (inp,stg1,out) 

allocate :: Int -> Work -> Work 
allocate k = \(inp,stg,out) -> 
    let beg = [Nothing | _ <- [1..k]]
    in (inp, beg++stg,out)

getBase :: Work -> Int 
getBase = \(_,stg,_) -> length stg 

free :: Int -> Work -> Work
free k = \(inp, stg,out) -> (inp, drop k stg,out)

writeValue :: Integer -> Work -> Work 
writeValue v = \(inp, stg, out) -> (inp, stg, out ++ [v])

readInput :: Work -> Integer
readInput = \(inp,_,_) -> if null inp then error "readInput" else head inp 

dropInput :: Work -> Work 
dropInput = \(inp,stg,out) -> (tail inp,stg,out)

applyBo :: Op -> Integer -> Integer -> Integer 
applyBo Plus v1 v2  = v1 + v2  
applyBo Minus v1 v2 = v1 - v2
applyBo Times v1 v2 = v1 * v2
applyBo Div v1 v2   = if v2 /= 0 then div v1 v2 else error "DivOnZero"
applyBo Mod v1 v2   = if v2 /= 0 then mod v1 v2 else error "ModOnZero" 

-------------------------------------
eExpr :: Expr -> Env -> Work -> Integer
eExpr (Var s) env          = \w -> getValue (getLocation s env) w    -- getDValue s env w
eExpr (Const v) _          = \_ -> v 
eExpr (BinOp op e1 e2) env = \w -> applyBo op (eExpr e1 env w) (eExpr e2 env w) 

eProc :: Proc -> Env -> Denote
eProc (ps,st) env = Procedure $ \ds -> iStmt st ((zip ps ds)++env) 

extEnv :: [String] -> [(String,Proc)] -> Int -> Env -> Env
extEnv vs ps b = \env -> let nenv1 = (zip vs [Location (b+i) | i<- [1..]]) ++ env             
                             nenv  = (map (\(n,pr) -> (n, eProc pr nenv)) ps) ++ nenv1
                         in nenv

iStmt :: Stmt -> Env -> Work -> Work 
iStmt (Assign var e) env = \w -> updValue (getLocation var env)(eExpr e env w) w   
iStmt (If e s) env       = \w -> if eExpr e env w > 0 then iStmt s env w else w
iStmt wh@(While e s) env = \w -> if eExpr e env w > 0 then iStmt wh env (iStmt s env w) else w 
iStmt (Call p vs) env    = \w -> let f  = getProcedure p env       
                                     ds = map (flip getDenote env) vs  
                                 in f ds w
iStmt (Block vs nps sts) env 
             = \w -> let k     = length vs 
                         w1    = allocate k w   
                         nenv = extEnv vs nps (getBase w) env
                         w2 = foldl (\wr s -> iStmt s nenv wr) w1 sts               
                     in free k w2
iStmt (Read var) env     = \w -> let v = readInput w
                                     w1 = updValue (getLocation var env) v  w          
                                 in dropInput w1
iStmt (Write e) env      = \w -> writeValue (eExpr e env w) w

iProgram :: Program -> [Integer] -> [Integer]
iProgram prog ix = let w = (ix, [],[])          
                       (_,_,ox) = iStmt prog [] w  
                   in ox

--------------------- State -----------------
eExprS :: Expr -> Env -> State Work Integer
eExprS (Var s) env          = do w <- get 
                                 return $ getValue (getLocation s env) w  
eExprS (Const v) _          = return v 
eExprS (BinOp op e1 e2) env = do v1 <- eExprS e1 env
                                 v2 <- eExprS e2 env
                                 return $ applyBo op v1 v2 

eProcS :: Proc -> Env -> Denote
eProcS (ps,st) env = Procedure $ \ds -> execState $ iStmtS st ((zip ps ds)++env) 

extEnvS :: [String] -> [(String,Proc)] -> Int -> Env -> Env
extEnvS vs ps b = \env -> let nenv1 = (zip vs [Location (b+i) | i<- [1..]]) ++ env             
                              nenv  = (map (\(n,pr) -> (n, eProcS pr nenv)) ps) ++ nenv1
                          in nenv

iStmtS :: Stmt -> Env -> State Work () 
iStmtS (Assign var e) env = do v <- eExprS e env
                               modify (updValue (getLocation var env) v)   
iStmtS (If e s) env       = do v <- eExprS e env
                               if v > 0 then iStmtS s env else return ()
                           -- when (v>0) (iStmtS s)
iStmtS wh@(While e s) env = do v <- eExprS e env
                               if v > 0 then do iStmtS s env
                                                iStmtS wh env 
                                        else return ()
                           -- when (v>0) (iStmtS s >> iStmtS wh)
iStmtS (Call p vs) env    = let f = getProcedure p env   
                                ds = map (flip getDenote env) vs  
                            in  modify (f ds)
iStmtS (Block vs nps sts) env = do let k = length vs
                                   w <- get
                                   let nenv = extEnvS vs nps (getBase w) env
                                   modify (allocate k)
                                   mapM_ (flip iStmtS nenv)sts
                                   modify (free k)
iStmtS (Read var) env     = do w <- get 
                               let v = readInput w
                               modify (updValue (getLocation var env) v) 
                               modify dropInput
iStmtS (Write e) env      = do v <- eExprS e env
                               modify (writeValue v)

iProgramS :: Program -> [Integer] -> [Integer]
iProgramS prog ix = let (_,_,ox) = execState (iStmtS prog []) (ix,[],[])  
                    in ox

--------------------- ReaderT + State -----------------	
eExprRS :: Expr -> ReaderT Env (State Work) Integer
eExprRS (Var s)          = do w <- lift get
                              e <- ask 
                              return $ getValue (getLocation s e) w   
eExprRS (Const v)        = return v 
eExprRS (BinOp op e1 e2) = do v1 <- eExprRS e1 
                              v2 <- eExprRS e2
                              return $ applyBo op v1 v2  

eProcRS :: Proc -> Env -> Denote
eProcRS (ps,st) env = Procedure $ \ds -> execState $ runReaderT (iStmtRS st) ((zip ps ds)++env) 

extEnvRS :: [String] -> [(String,Proc)] -> Int -> Env -> Env
extEnvRS vs ps b = \env -> let nenv1 = (zip vs [Location (b+i) | i<- [1..]]) ++ env             
                               nenv  = (map (\(n,pr) -> (n, eProcRS pr nenv)) ps) ++ nenv1
                           in nenv


iStmtRS :: Stmt -> ReaderT Env (State Work) () 
iStmtRS (Assign var ex) = do v <- eExprRS ex
                             e <- ask 
                             lift $ modify (updValue (getLocation var e) v)   
iStmtRS (If e s)        = do v <- eExprRS e 
                             if v > 0 then iStmtRS s else return ()
                           -- when (v>0) (iStmtS s)
iStmtRS wh@(While e s)  = do v <- eExprRS e
                             if v > 0 then do iStmtRS s 
                                              iStmtRS wh 
                                      else return ()
                          -- when (v>0) (iStmtS s >> iStmtS wh)
iStmtRS (Call p vs)     = do e <- ask 
                             let f = getProcedure p e      
                             let ds = map (flip getDenote e) vs  
                             lift $ modify (f ds) 
iStmtRS (Block vs nps sts)
                        = do let k = length vs
                             w <- lift get
                             lift $ modify (allocate k)
                             local (extEnvRS vs nps (getBase w)) (mapM_ iStmtRS sts)
                             lift $ modify (free k)
iStmtRS (Read var)      = do w <- lift get 
                             let v = readInput w
                             e <- ask 
                             lift $ modify (updValue (getLocation var e) v)    
                             lift $ modify dropInput
iStmtRS (Write ex)      = do v <- eExprRS ex
                             lift $ modify (writeValue v)

iProgramRS :: Program -> [Integer] -> [Integer]
iProgramRS prog ix = let (_,_,ox) = execState (runReaderT (iStmtRS prog) []) (ix,[],[])  
                    in ox

--------------------- ReaderT + State applicative -----------------
eExprRSa :: Expr -> ReaderT Env (State Work) Integer
eExprRSa (Var s)          = (getLocation s <$> ask) >>= (\k -> lift get >>= return .(getValue k))    
eExprRSa (Const v)        = return v 
eExprRSa (BinOp op e1 e2) = (applyBo op) <$> eExprRSa e1 <*> eExprRSa e2

eProcRSa :: Proc -> Env -> Denote
eProcRSa (ps,st) env = Procedure $ \ds -> execState $ runReaderT (iStmtRSa st) ((zip ps ds)++env) 

extEnvRSa :: [String] -> [(String,Proc)] -> Int -> Env -> Env
extEnvRSa vs ps b = \env -> let nenv1 = (zip vs [Location (b+i) | i<- [1..]]) ++ env             
                                nenv  = (map (\(n,pr) -> (n, eProcRSa pr nenv)) ps) ++ nenv1
                            in nenv

iStmtRSa :: Stmt -> ReaderT Env (State Work)()  
iStmtRSa (Assign var e)= (getLocation var <$> ask) >>= (\k -> eExprRSa e >>= (\v -> (lift . modify) (updValue k v) ))
iStmtRSa (If e s)      = eExprRSa e >>= \v -> when (v>0) (iStmtRSa s)
iStmtRSa wh@(While e s)= eExprRSa e >>= \v -> when (v>0) (iStmtRSa s >> iStmtRSa wh)
iStmtRSa (Call p vs)   = ask >>= \e -> lift $ modify (getProcedure p e $ map (flip getDenote e) vs)
iStmtRSa (Block vs nps sts) = lift get >>= 
                        (\w -> (lift . modify) (allocate (length vs))
                               >> local (extEnvRSa vs nps (getBase w)) (mapM_ iStmtRSa sts)
                                  >> (lift . modify) (free (length vs)) )
iStmtRSa (Read var)    = readInput <$> lift get >>= 
                             (\v -> getLocation var <$> ask >>=(\k -> (lift . modify) (updValue k v))) >> 
                                  (lift.modify) dropInput
iStmtRSa (Write e)     = eExprRSa e >>= (lift . modify) . writeValue

iProgramRSa :: Program -> [Integer] -> [Integer]
iProgramRSa prog ix = let (_,_,ox) = execState (runReaderT (iStmtRSa prog) []) (ix,[],[])  
                      in ox

iProgramMain :: SemanOpt -> Program -> [Integer] -> [Integer]  --   WorkO | StateO | ReaderO | ApplicO
iProgramMain r p il = case r of 
     ApplicO -> iProgramRSa p il 
     ReaderO -> iProgramRS p il 
     StateO  -> iProgramS p il 
     WorkO   -> iProgram p il

