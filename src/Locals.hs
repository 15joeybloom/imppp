module Locals where

import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map.Strict         as M

type M =
    ReaderT Env (StateT Store (Except ImpError))

type Name = String

type Loc = Int

newtype Env = Env { getEnv :: M.Map Name Loc }

newtype Store = Store { getStore :: M.Map Loc Integer }
    deriving Show

data ImpError = ImpError
    { env :: Env
    , store :: Store
    , message :: String
    }

impError :: String -> M a
impError message = do
    env <- ask
    store <- get
    throwError $ ImpError { env, store, message }

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
    | Declare [Name]

alloc :: M Loc
alloc = do
    Store store <- get
    let fresh = M.size store
    modify $ Store . M.insert fresh 0 . getStore
    return fresh

storeLookup :: Loc -> M Integer
storeLookup loc = do
    Store store <- get
    case M.lookup loc store of
        Nothing -> error "dangling pointer!"
        Just i -> return i

envLookup :: Name -> M Loc
envLookup x = do
    Env env <- ask
    case M.lookup x env of
        Nothing -> impError $ x ++ " uninitialized!"
        Just loc -> return loc

interpAExp :: AExp -> M Integer
interpAExp (I i) = return i
interpAExp (Var x) = envLookup x >>= storeLookup
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
        then impError "Division by zero!"
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
    loc <- envLookup x
    _ <- storeLookup loc -- XXX: detect dangling pointers
    i <- interpAExp e
    modify $ Store . M.insert loc i . getStore
interpStmt (If c t e) = do
    b <- interpBExp c
    interpStmt $ if b then t else e
interpStmt while@(While c s) =
    interpStmt $ If c (Stmts [s, while]) (Stmts [])
interpStmt (Stmts stmts) = case stmts of
    [] -> return ()
    (Declare (x:xs)) : rest -> do
        fresh <- alloc
        local (Env . M.insert x fresh . getEnv)
            $ interpStmt $ Stmts $ Declare xs : rest
    s : rest -> do
        interpStmt s
        interpStmt $ Stmts rest
interpStmt (Declare _) = return ()

-- XXX: It doesn't make a ton of sense to return the store since there is no
-- more notion of global variables. But I need some way to test programs, so
-- this will have to do.
runImp :: Stmt -> Either ImpError Store
runImp program = runExcept except
  where
    except = execStateT stateT $ Store M.empty
    stateT = runReaderT (interpStmt program) (Env M.empty)
