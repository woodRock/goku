module Language.Evaluator
    ( eval
    , evalStmt
    , evalProgram
    , GokuError(..)
    ) where

import Language.Syntax
import Language.Context
import Control.Exception (throwIO, Exception)

-- Define a custom exception type for runtime errors
data GokuError = GokuError String deriving (Show)

instance Exception GokuError

-- A simple evaluator for the language
eval :: Expr -> Context -> IO (Expr, Context)
eval (Var name) ctx = case lookup name ctx of
  Just val -> return (val, ctx)
  Nothing -> throwIO $ GokuError ("Unbound variable: " ++ name)
eval (Lam name t body) ctx = return (Lam name t body, ctx)
eval (App func arg) ctx = do
  (evalFunc, ctx') <- eval func ctx
  (evalArg, ctx'') <- eval arg ctx'
  case evalFunc of
    Lam name _ body -> eval body ((name, evalArg) : ctx'')
    _ -> throwIO $ GokuError "Cannot apply non-function"
eval (LitInt n) ctx = return (LitInt n, ctx)
eval (LitBool b) ctx = return (LitBool b, ctx)

-- A simple statement evaluator for the language
evalStmt :: Stmt -> Context -> IO Context
evalStmt (ExprStmt expr) ctx = do
  (_, ctx') <- eval expr ctx
  return ctx'
evalStmt (Let name _ expr) ctx = do
  (val, ctx') <- eval expr ctx
  return ((name, val) : ctx')
evalStmt (If cond thenBranch elseBranch) ctx = do
  (val, ctx') <- eval cond ctx
  case val of
    LitBool True -> evalStmt thenBranch ctx'
    LitBool False -> evalStmt elseBranch ctx'
    _ -> throwIO $ GokuError "If condition must be a boolean"
evalStmt (While cond body) ctx = do
  (val, ctx') <- eval cond ctx
  case val of
    LitBool True -> do
      ctx'' <- evalStmt body ctx'
      evalStmt (While cond body) ctx''
    LitBool False -> return ctx'
    _ -> throwIO $ GokuError "While condition must be a boolean"
evalStmt (Return expr) ctx = do
  (_, ctx') <- eval expr ctx
  return ctx'
evalStmt (Assert expr) ctx = do
  (val, ctx') <- eval expr ctx
  case val of
    LitBool True -> return ctx'
    LitBool False -> throwIO $ GokuError "Assertion failed!"
    _ -> throwIO $ GokuError "Assertion must be a boolean"

-- A simple program evaluator for the language
evalProgram :: Program -> Context -> IO Context
evalProgram (Program []) ctx = return ctx
evalProgram (Program (stmt:stmts)) ctx = do
  ctx' <- evalStmt stmt ctx
  evalProgram (Program stmts) ctx'