{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

module Imppp where

import Prelude hiding (print)

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Reader
import           Control.Monad.State hiding (state)
import           Control.Monad.Writer
import qualified Data.Map.Strict         as M

type M =
    MaybeT (ReaderT Env (StateT ImpState (ExceptT ImpError (Writer String))))

type Name = String

type Loc = Int

newtype Env = Env { getEnv :: M.Map Name Loc }

data ImpState = ImpState
    { input :: ![Integer]
    , store :: !(M.Map Loc Integer)
    }
    deriving Show

data ImpError = ImpError
    { env :: !Env
    , state :: !ImpState
    , message :: !String
    }

impError :: String -> M a
impError message = do
    env <- ask
    state <- get
    throwError $ ImpError { env, state, message }

makeLensesFor [("input", "inputLens"), ("store", "storeLens")] ''ImpState

data AExp
    = I !Integer
    | Var !Name
    | Negate !AExp
    | Div !AExp !AExp
    | Plus !AExp !AExp
    | Inc !Name
    | Read

data BExp
    = B !Bool
    | Leq !AExp !AExp
    | Not !BExp
    | And !BExp !BExp

data Printable = forall a. PrintableClass a => Printable !a
class PrintableClass a where
    print :: a -> M ()
instance PrintableClass String where
    print = tell
instance PrintableClass AExp where
    print e = interpAExp e >>= tell . show

data Stmt
    = Assign !Name !AExp
    | If !BExp !Stmt !Stmt
    | While !BExp !Stmt
    | Stmts ![Stmt]
    | Print ![Printable]
    | Declare ![Name]
    | Halt

alloc :: M Loc
alloc = do
    store <- use storeLens
    let fresh = M.size store
    storeLens %= M.insert fresh 0
    return fresh

storeLookup :: Loc -> M Integer
storeLookup loc = do
    store <- use storeLens
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
interpAExp (Inc x) = do
    loc <- envLookup x
    storeLens %= M.adjust (+1) loc
    storeLookup loc
interpAExp Read = do
    input <- use inputLens
    case input of
        [] -> impError "Not enough input!"
        (x:xs) -> do
            inputLens .= xs
            return x

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
    storeLens %= M.insert loc i
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
interpStmt (Print items) = mapM_ (\(Printable p) -> print p) items
interpStmt Halt = MaybeT $ return Nothing

runImp :: Stmt -> [Integer] -> (Maybe ImpError, String)
runImp program input = runWriter $ fmap onlyError $ runExceptT $ except
  where
    startState = ImpState { store = M.empty , input }
    onlyError = \case
        Left x -> Just x
        Right _ -> Nothing
    except = execStateT stateT startState
    stateT = runReaderT readerT (Env M.empty)
    readerT = runMaybeT $ interpStmt program
-- XXX: It doesn't make a ton of sense to return the store since there is no
-- more notion of global variables. But I need some way to test programs, so
-- this will have to do.
