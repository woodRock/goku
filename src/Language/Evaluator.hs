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
    Lam name _ body -> do
      -- Create a new context with the argument bound to the lambda's parameter
      let newCtx = (name, evalArg) : ctx''
      -- Evaluate the lambda's body in the new context
      eval body newCtx
    _ -> throwIO $ GokuError "Cannot apply non-function"
eval (LitInt n) ctx = return (LitInt n, ctx)
eval (LitBool b) ctx = return (LitBool b, ctx)
eval (Equals e1 e2) ctx = do
  (val1, ctx') <- eval e1 ctx
  (val2, ctx'') <- eval e2 ctx'
  case (val1, val2) of
    (LitInt n1, LitInt n2) -> return (LitBool (n1 == n2), ctx'')
    (LitBool b1, LitBool b2) -> return (LitBool (b1 == b2), ctx'')
    _ -> throwIO $ GokuError "Cannot compare values of different types or non-primitive types"
eval (Add e1 e2) ctx = do
  (val1, ctx') <- eval e1 ctx
  (val2, ctx'') <- eval e2 ctx'
  case (val1, val2) of
    (LitInt n1, LitInt n2) -> return (LitInt (n1 + n2), ctx'')
    _ -> throwIO $ GokuError "Cannot add non-integer values"
eval (LessThan e1 e2) ctx = do
  (val1, ctx') <- eval e1 ctx
  (val2, ctx'') <- eval e2 ctx'
  case (val1, val2) of
    (LitInt n1, LitInt n2) -> return (LitBool (n1 < n2), ctx'')
    _ -> throwIO $ GokuError "Cannot compare non-integer values with <"

-- A simple statement evaluator for the language
evalStmt :: Stmt -> Context -> IO Context
evalStmt (ExprStmt expr) ctx = do
  (_, ctx') <- eval expr ctx
  return ctx'
evalStmt (Let name _ expr) ctx = do
  (val, ctx') <- eval expr ctx
  return ((name, val) : ctx')
evalStmt (Set name expr) ctx = do
  (val, ctx') <- eval expr ctx
  newCtx <- updateContext name val ctx'
  return newCtx
  where
    updateContext :: String -> Expr -> Context -> IO Context
    updateContext name val [] = throwIO $ GokuError ("Unbound variable for set: " ++ name)
    updateContext name val ((n, v):rest) = 
      if n == name then return ((name, val) : rest)
      else do
        updatedRest <- updateContext name val rest
        return ((n, v) : updatedRest)
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
      -- Re-evaluate the while loop with the updated context
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