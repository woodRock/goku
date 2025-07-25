module Language.Types
    ( TypedExpr(..)
    , inferType
    , typeCheck
    , TypeEnv
    ) where

import Language.Syntax

-- | Expression with type information
data TypedExpr = TypedExpr
    { exprType :: Type
    , expr :: Expr
    } deriving (Show, Eq)

-- | Type environment for variables (simple association list)
type TypeEnv = [(String, Type)]

-- | Infer the type of an expression
inferType :: TypeEnv -> Expr -> Either String Type
inferType _ (LitInt _) = Right TInt
inferType _ (LitBool _) = Right TBool
inferType _ (LitString _) = Right TString
inferType env (Var name) = 
    case lookup name env of
        Just t -> Right t
        Nothing -> Left $ "Unbound variable: " ++ name
inferType env (Add e1 e2) = do
    t1 <- inferType env e1
    t2 <- inferType env e2
    case (t1, t2) of
        (TInt, TInt) -> Right TInt
        _ -> Left "Addition requires integer operands"
inferType env (Concat e1 e2) = do
    t1 <- inferType env e1
    t2 <- inferType env e2
    case (t1, t2) of
        (TString, TString) -> Right TString
        _ -> Left "Concatenation requires string operands"
inferType env (Equals e1 e2) = do
    t1 <- inferType env e1
    t2 <- inferType env e2
    if t1 == t2
        then Right TBool
        else Left $ "Cannot compare different types: " ++ show t1 ++ " and " ++ show t2
-- Add more cases as needed...
inferType _ _ = Left "Type inference not implemented for this expression"

-- | Type check an expression against an expected type
typeCheck :: TypeEnv -> Expr -> Type -> Either String TypedExpr
typeCheck env e expectedType = do
    actualType <- inferType env e
    if actualType == expectedType
        then Right $ TypedExpr actualType e
        else Left $ "Type mismatch: expected " ++ show expectedType ++ 
                   " but got " ++ show actualType