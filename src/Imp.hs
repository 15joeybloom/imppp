module Imp where

import           Control.Monad.State
import qualified Data.Map.Strict         as M

type M =
    State Store

type Name = String

newtype Store = Store { getStore :: M.Map Name Integer }
    deriving Show

data AExp
    = I Integer
    | Var Name
    | Negate AExp
    | Div AExp AExp
    | Plus AExp AExp

data BExp
    = B Bool
    | Leq AExp AExp
    | Not BExp
    | And BExp BExp

data Stmt
    = Assign Name AExp
    | If BExp Stmt Stmt
    | While BExp Stmt
    | Stmts [Stmt]

data Pgm
    = Pgm [Name] Stmt

interpAExp :: AExp -> M Integer
interpAExp (I i) = return i
interpAExp (Var x) = do
    Store store <- get
    case M.lookup x store of
        Just i -> return i
        Nothing -> error $ x ++ " uninitialized!"
interpAExp (Negate e) = do
    i <- interpAExp e
    return $ -i
interpAExp (Plus e1 e2) = do
    i1 <- interpAExp e1
    i2 <- interpAExp e2
    return $ i1 + i2
interpAExp (Div e1 e2) = do
    i1 <- interpAExp e1
    i2 <- interpAExp e2
    if i2 == 0
        then error "Division by zero!"
        else return $ quot i1 i2

interpBExp :: BExp -> M Bool
interpBExp (B b) = return b
interpBExp (Leq e1 e2) = do
    i1 <- interpAExp e1
    i2 <- interpAExp e2
    return $ i1 <= i2
interpBExp (Not e) = do
    b <- interpBExp e
    return $ not b
interpBExp (And e1 e2) = do
    b1 <- interpBExp e1
    if b1
        then interpBExp e2
        else return False

interpStmt :: Stmt -> M ()
interpStmt (Assign x e) = do
    i <- interpAExp e
    modify $ Store . M.insert x i . getStore
interpStmt (If c t e) = do
    b <- interpBExp c
    interpStmt $ if b then t else e
interpStmt while@(While c s) =
    interpStmt $ If c (Stmts [s, while]) (Stmts [])
interpStmt (Stmts []) = return ()
interpStmt (Stmts (s:rest)) = do
    interpStmt s
    interpStmt $ Stmts rest

runPgm :: Pgm -> Store
runPgm (Pgm ids body) = execState
    (interpStmt body)
    (Store $ M.fromList [(x, 0) | x <- ids])
