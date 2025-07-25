module Language.Evaluator
    ( eval
    , evalStmt
    , evalProgram
    , GokuError(..)
    ) where

import Language.Syntax
import Language.Context
import Control.Exception (throwIO, Exception)
import Data.List (intercalate)
import System.IO (hFlush, stdout)

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
eval (LitString s) ctx = return (LitString s, ctx)
eval (Equals e1 e2) ctx = do
  (val1, ctx') <- eval e1 ctx
  (val2, ctx'') <- eval e2 ctx'
  case (val1, val2) of
    (LitInt n1, LitInt n2) -> return (LitBool (n1 == n2), ctx'')
    (LitBool b1, LitBool b2) -> return (LitBool (b1 == b2), ctx'')
    (LitString s1, LitString s2) -> return (LitBool (s1 == s2), ctx'')
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
eval (Mod e1 e2) ctx = do
  (val1, ctx') <- eval e1 ctx
  (val2, ctx'') <- eval e2 ctx'
  case (val1, val2) of
    (LitInt n1, LitInt n2) -> 
      if n2 == 0 
      then throwIO $ GokuError "Modulo by zero"
      else return (LitInt (n1 `mod` n2), ctx'')  -- Modulo operation
    _ -> throwIO $ GokuError "Cannot take modulo of non-integer values"
eval (LessThan e1 e2) ctx = do
  (val1, ctx') <- eval e1 ctx
  (val2, ctx'') <- eval e2 ctx'
  case (val1, val2) of
    (LitInt n1, LitInt n2) -> return (LitBool (n1 < n2), ctx'')
    _ -> throwIO $ GokuError "Cannot compare non-integer values with <"
eval (LessThanEqual e1 e2) ctx = do
  (val1, ctx') <- eval e1 ctx
  (val2, ctx'') <- eval e2 ctx'
  case (val1, val2) of
    (LitInt n1, LitInt n2) -> return (LitBool (n1 <= n2), ctx'')
    _ -> throwIO $ GokuError "Cannot compare non-integer values with <="
eval (GreaterThan e1 e2) ctx = do
  (val1, ctx') <- eval e1 ctx
  (val2, ctx'') <- eval e2 ctx'
  case (val1, val2) of
    (LitInt n1, LitInt n2) -> return (LitBool (n1 > n2), ctx'')
    _ -> throwIO $ GokuError "Cannot compare non-integer values with >"
eval (GreaterThanEqual e1 e2) ctx = do
  (val1, ctx') <- eval e1 ctx
  (val2, ctx'') <- eval e2 ctx'
  case (val1, val2) of
    (LitInt n1, LitInt n2) -> return (LitBool (n1 >= n2), ctx'')
    _ -> throwIO $ GokuError "Cannot compare non-integer values with >="
eval (NotEquals e1 e2) ctx = do
  (val1, ctx') <- eval e1 ctx
  (val2, ctx'') <- eval e2 ctx'
  case (val1, val2) of
    (LitInt n1, LitInt n2) -> return (LitBool (n1 /= n2), ctx'')
    (LitBool b1, LitBool b2) -> return (LitBool (b1 /= b2), ctx'')
    (LitString s1, LitString s2) -> return (LitBool (s1 /= s2), ctx'')
    _ -> throwIO $ GokuError "Cannot compare different types"
eval (Concat e1 e2) ctx = do
  (val1, ctx') <- eval e1 ctx
  (val2, ctx'') <- eval e2 ctx'
  case (val1, val2) of
    (LitString s1, LitString s2) -> return (LitString (s1 ++ s2), ctx'')
    (LitList xs1, LitList xs2) -> return (LitList (xs1 ++ xs2), ctx'')
    (LitString _, _) -> throwIO $ GokuError "Cannot concatenate string with non-string"
    (LitList _, _) -> throwIO $ GokuError "Cannot concatenate list with non-list"
    _ -> throwIO $ GokuError "Cannot concatenate non-string, non-list values"
eval (IfExpr cond thenExpr elseExpr) ctx = do
  (condVal, ctx') <- eval cond ctx
  case condVal of
    LitBool True -> eval thenExpr ctx'
    LitBool False -> eval elseExpr ctx'
    _ -> throwIO $ GokuError "If condition must be a boolean"
eval (LitList elements) ctx = do
  (evalElements, ctx') <- evalList elements ctx
  return (LitList evalElements, ctx')
  where
    evalList [] c = return ([], c)
    evalList (e:es) c = do
      (evalE, c') <- eval e c
      (evalEs, c'') <- evalList es c'
      return (evalE : evalEs, c'')
eval (ListCons e1 e2) ctx = do
  (val1, ctx') <- eval e1 ctx
  (val2, ctx'') <- eval e2 ctx'
  case val2 of
    LitList xs -> return (LitList (val1 : xs), ctx'')
    _ -> throwIO $ GokuError "Cannot cons to non-list"
eval (ListHead e) ctx = do
  (val, ctx') <- eval e ctx
  case val of
    LitList [] -> throwIO $ GokuError "Cannot take head of empty list"
    LitList (x:_) -> return (x, ctx')
    _ -> throwIO $ GokuError "Cannot take head of non-list"
eval (ListTail e) ctx = do
  (val, ctx') <- eval e ctx
  case val of
    LitList [] -> throwIO $ GokuError "Cannot take tail of empty list"
    LitList (_:xs) -> return (LitList xs, ctx')
    _ -> throwIO $ GokuError "Cannot take tail of non-list"
eval (ListEmpty e) ctx = do
  (val, ctx') <- eval e ctx
  case val of
    LitList [] -> return (LitBool True, ctx')
    LitList _ -> return (LitBool False, ctx')
    _ -> throwIO $ GokuError "Cannot check emptiness of non-list"
eval (ListLength e) ctx = do
  (val, ctx') <- eval e ctx
  case val of
    LitList xs -> return (LitInt (length xs), ctx')
    _ -> throwIO $ GokuError "Cannot get length of non-list"
eval (ListAppend listExpr elemExpr) ctx = do
  (listVal, ctx1) <- eval listExpr ctx
  (elemVal, ctx2) <- eval elemExpr ctx1
  case listVal of
    LitList xs -> return (LitList (xs ++ [elemVal]), ctx2)
    _ -> throwIO $ GokuError "Cannot append to non-list"
eval (ListNth listExpr indexExpr) ctx = do
  (listVal, ctx1) <- eval listExpr ctx
  (indexVal, ctx2) <- eval indexExpr ctx1
  case (listVal, indexVal) of
    (LitList xs, LitInt i) -> 
      if i >= 0 && i < length xs
        then return (xs !! i, ctx2)
        else throwIO $ GokuError $ "Index " ++ show i ++ " out of bounds for list of length " ++ show (length xs)
    (_, LitInt _) -> throwIO $ GokuError "Cannot index non-list"
    (LitList _, _) -> throwIO $ GokuError "List index must be an integer"
    _ -> throwIO $ GokuError "Cannot index non-list with non-integer"
eval (ListReverse e) ctx = do
  (val, ctx') <- eval e ctx
  case val of
    LitList xs -> return (LitList (reverse xs), ctx')
    _ -> throwIO $ GokuError "Cannot reverse non-list"
eval (ListElem elemExpr listExpr) ctx = do
  (elemVal, ctx1) <- eval elemExpr ctx
  (listVal, ctx2) <- eval listExpr ctx1
  case listVal of
    LitList xs -> return (LitBool (elemVal `elem` xs), ctx2)
    _ -> throwIO $ GokuError "Cannot check membership in non-list"

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
    updateContext varName varVal [] = return [(varName, varVal)]  -- Create new binding if not found
    updateContext varName varVal ((n, v):rest) = 
      if n == varName then return ((varName, varVal) : rest)
      else do
        updatedRest <- updateContext varName varVal rest
        return ((n, v) : updatedRest)
evalStmt (Block stmts) ctx = evalStmts stmts ctx
evalStmt (If cond thenBranch maybeElseBranch) ctx = do
  (val, ctx') <- eval cond ctx
  case val of
    LitBool True -> evalStmt thenBranch ctx'
    LitBool False -> case maybeElseBranch of
        Just elseBranch -> evalStmt elseBranch ctx'
        Nothing -> return ctx'  -- No else branch, just return context
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
evalStmt (Print expr) ctx = do
  (val, ctx') <- eval expr ctx
  putStrLn $ prettyPrintValue val
  hFlush stdout
  return ctx'
  where
    prettyPrintValue :: Expr -> String
    prettyPrintValue (LitInt n) = show n
    prettyPrintValue (LitBool True) = "true"
    prettyPrintValue (LitBool False) = "false"
    prettyPrintValue (LitString s) = s  -- Print strings without quotes
    prettyPrintValue (LitList exprs) = "[" ++ intercalate ", " (map prettyPrintValue exprs) ++ "]"
    prettyPrintValue otherExpr = show otherExpr

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