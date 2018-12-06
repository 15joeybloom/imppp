{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

module IO where

import Prelude hiding (print)

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State hiding (state)
import           Control.Monad.Writer
import qualified Data.Map.Strict         as M

type M =
    StateT ImpState (ExceptT ImpError (Writer String))

type Name = String

data ImpState = ImpState
    { input :: ![Integer]
    , store :: !(M.Map Name Integer)
    }
    deriving Show

data ImpError = ImpError
    { state :: !ImpState
    , message :: !String
    }

impError :: String -> M a
impError message = do
    state <- get
    throwError $ ImpError { state, message }

makeLensesFor [("input", "inputLens"), ("store", "storeLens")] ''ImpState

data AExp
    = I !Integer
    | Var !Name
    | Negate !AExp
    | Div !AExp !AExp
    | Plus !AExp !AExp
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

data Pgm
    = Pgm ![Name] !Stmt

storeLookup :: Name -> M Integer
storeLookup x = do
    store <- use storeLens
    case M.lookup x store of
        Just i -> return i
        Nothing -> impError $ x ++ " uninitialized!"

interpAExp :: AExp -> M Integer
interpAExp (I i) = return i
interpAExp (Var x) = storeLookup x
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
    _ <- storeLookup x --make sure x is declared
    i <- interpAExp e
    storeLens %= M.insert x i
interpStmt (If c t e) = do
    b <- interpBExp c
    interpStmt $ if b then t else e
interpStmt while@(While c s) =
    interpStmt $ If c (Stmts [s, while]) (Stmts [])
interpStmt (Stmts []) = return ()
interpStmt (Stmts (s:rest)) = do
    interpStmt s
    interpStmt $ Stmts rest
interpStmt (Print items) = mapM_ (\(Printable p) -> print p) items

runPgm :: Pgm -> [Integer] -> (Either ImpError ImpState, String)
runPgm (Pgm ids body) input = runWriter $ runExceptT $ execStateT
    (interpStmt body) startState
  where
    startState = ImpState
        { store = M.fromList [(x, 0) | x <- ids]
        , input
        }
