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
eval (Lam params t body) ctx = return (Lam params t body, ctx)
eval (App func arg) ctx = do
  (evalFunc, ctx') <- eval func ctx
  (evalArg, ctx'') <- eval arg ctx'
  case evalFunc of
    Lam [] _ body -> eval body ctx'' -- No parameters left, evaluate body
    Lam (name:restParams) funcType body -> do
      -- Create a new context with the argument bound to the first parameter
      let newCtx = (name, evalArg) : ctx''
      -- If there are more parameters, create a partial application
      case restParams of
        [] -> eval body newCtx -- This was the last parameter, evaluate body
        _ -> return (Lam restParams funcType body, newCtx) -- Return partial application
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
eval (Sub e1 e2) ctx = do
  (val1, ctx') <- eval e1 ctx
  (val2, ctx'') <- eval e2 ctx'
  case (val1, val2) of
    (LitInt n1, LitInt n2) -> return (LitInt (n1 - n2), ctx'')
    _ -> throwIO $ GokuError "Cannot subtract non-integer values"
eval (Mult e1 e2) ctx = do
  (val1, ctx') <- eval e1 ctx
  (val2, ctx'') <- eval e2 ctx'
  case (val1, val2) of
    (LitInt n1, LitInt n2) -> return (LitInt (n1 * n2), ctx'')
    _ -> throwIO $ GokuError "Cannot multiply non-integer values"
eval (Div e1 e2) ctx = do
  (val1, ctx') <- eval e1 ctx
  (val2, ctx'') <- eval e2 ctx'
  case (val1, val2) of
    (LitInt n1, LitInt n2) -> 
      if n2 == 0 
      then throwIO $ GokuError "Division by zero"
      else return (LitInt (n1 `div` n2), ctx'')  -- Integer division for now
    _ -> throwIO $ GokuError "Cannot divide non-integer values"
eval (IntDiv e1 e2) ctx = do
  (val1, ctx') <- eval e1 ctx
  (val2, ctx'') <- eval e2 ctx'
  case (val1, val2) of
    (LitInt n1, LitInt n2) -> 
      if n2 == 0 
      then throwIO $ GokuError "Division by zero"
      else return (LitInt (n1 `div` n2), ctx'')  -- Floor division (rounds toward negative infinity)
    _ -> throwIO $ GokuError "Cannot divide non-integer values"
eval (LessThan e1 e2) ctx = do
  (val1, ctx') <- eval e1 ctx
  (val2, ctx'') <- eval e2 ctx'
  case (val1, val2) of
    (LitInt n1, LitInt n2) -> return (LitBool (n1 < n2), ctx'')
    _ -> throwIO $ GokuError "Cannot compare non-integer values with <"
eval (IfExpr cond thenExpr elseExpr) ctx = do
  (condVal, ctx') <- eval cond ctx
  case condVal of
    LitBool True -> eval thenExpr ctx'
    LitBool False -> eval elseExpr ctx'
    _ -> throwIO $ GokuError "If condition must be a boolean"

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
    updateContext varName _varVal [] = throwIO $ GokuError ("Unbound variable for set: " ++ varName)
    updateContext varName varVal ((n, v):rest) = 
      if n == varName then return ((varName, varVal) : rest)
      else do
        updatedRest <- updateContext varName varVal rest
        return ((n, v) : updatedRest)
evalStmt (Block stmts) ctx = evalStmts stmts ctx
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

-- Helper function to evaluate a list of statements
evalStmts :: [Stmt] -> Context -> IO Context
evalStmts [] ctx = return ctx
evalStmts (stmt:rest) ctx = do
  ctx' <- evalStmt stmt ctx
  evalStmts rest ctx'

-- A simple program evaluator for the language
evalProgram :: Program -> Context -> IO Context
evalProgram (Program []) ctx = return ctx
evalProgram (Program (stmt:stmts)) ctx = do
  ctx' <- evalStmt stmt ctx
  evalProgram (Program stmts) ctx'