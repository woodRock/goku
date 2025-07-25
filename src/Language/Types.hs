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
inferType env (Sub e1 e2) = do
    t1 <- inferType env e1
    t2 <- inferType env e2
    case (t1, t2) of
        (TInt, TInt) -> Right TInt
        _ -> Left "Subtraction requires integer operands"
inferType env (Mult e1 e2) = do
    t1 <- inferType env e1
    t2 <- inferType env e2
    case (t1, t2) of
        (TInt, TInt) -> Right TInt
        _ -> Left "Multiplication requires integer operands"
inferType env (Div e1 e2) = do
    t1 <- inferType env e1
    t2 <- inferType env e2
    case (t1, t2) of
        (TInt, TInt) -> Right TInt
        _ -> Left "Division requires integer operands"
inferType env (IntDiv e1 e2) = do
    t1 <- inferType env e1
    t2 <- inferType env e2
    case (t1, t2) of
        (TInt, TInt) -> Right TInt
        _ -> Left "Integer division requires integer operands"
inferType env (Mod e1 e2) = do
    t1 <- inferType env e1
    t2 <- inferType env e2
    case (t1, t2) of
        (TInt, TInt) -> Right TInt
        _ -> Left "Modulo requires integer operands"
inferType env (Concat e1 e2) = do
    t1 <- inferType env e1
    t2 <- inferType env e2
    case (t1, t2) of
        (TString, TString) -> Right TString
        (TList elemType1, TList elemType2) ->
            if elemType1 == elemType2
                then Right (TList elemType1)
                else Left $ "Cannot concatenate lists of different types: " ++ show elemType1 ++ " and " ++ show elemType2
        (TString, _) -> Left "Cannot concatenate string with non-string"
        (TList _, _) -> Left "Cannot concatenate list with non-list"
        _ -> Left "Concatenation requires string or list operands"
inferType env (Equals e1 e2) = do
    t1 <- inferType env e1
    t2 <- inferType env e2
    if t1 == t2
        then Right TBool
        else Left $ "Cannot compare different types: " ++ show t1 ++ " and " ++ show t2
inferType env (NotEquals e1 e2) = do
    t1 <- inferType env e1
    t2 <- inferType env e2
    if t1 == t2
        then Right TBool
        else Left $ "Cannot compare different types: " ++ show t1 ++ " and " ++ show t2
inferType env (LessThan e1 e2) = do
    t1 <- inferType env e1
    t2 <- inferType env e2
    case (t1, t2) of
        (TInt, TInt) -> Right TBool
        _ -> Left "Less-than comparison requires integer operands"
inferType env (LessThanEqual e1 e2) = do
    t1 <- inferType env e1
    t2 <- inferType env e2
    case (t1, t2) of
        (TInt, TInt) -> Right TBool
        _ -> Left "Less-than-equal comparison requires integer operands"
inferType env (GreaterThan e1 e2) = do
    t1 <- inferType env e1
    t2 <- inferType env e2
    case (t1, t2) of
        (TInt, TInt) -> Right TBool
        _ -> Left "Greater-than comparison requires integer operands"
inferType env (GreaterThanEqual e1 e2) = do
    t1 <- inferType env e1
    t2 <- inferType env e2
    case (t1, t2) of
        (TInt, TInt) -> Right TBool
        _ -> Left "Greater-than-equal comparison requires integer operands"
-- List type inference
inferType _ (LitList []) = Right (TList TInt)  -- Empty list defaults to list of integers
inferType env (LitList (e:es)) = do
    elemType <- inferType env e
    -- Check all elements have the same type
    let checkRestTypes [] = Right elemType
        checkRestTypes (x:xs) = do
            t <- inferType env x
            if t == elemType
                then checkRestTypes xs
                else Left $ "All list elements must have the same type. Expected " ++ show elemType ++ " but found " ++ show t
    _ <- checkRestTypes es
    Right (TList elemType)
inferType env (ListCons e listExpr) = do
    elemType <- inferType env e
    listType <- inferType env listExpr
    case listType of
        TList expectedElemType ->
            if elemType == expectedElemType
                then Right (TList elemType)
                else Left $ "Cannot cons " ++ show elemType ++ " to list of " ++ show expectedElemType
        _ -> Left "Cons (::) requires a list as the second argument"
inferType env (ListHead listExpr) = do
    listType <- inferType env listExpr
    case listType of
        TList elemType -> Right elemType
        _ -> Left "head requires a list argument"
inferType env (ListTail listExpr) = do
    listType <- inferType env listExpr
    case listType of
        TList elemType -> Right (TList elemType)
        _ -> Left "tail requires a list argument"
inferType env (ListEmpty listExpr) = do
    listType <- inferType env listExpr
    case listType of
        TList _ -> Right TBool
        _ -> Left "empty requires a list argument"
inferType env (ListLength listExpr) = do
    listType <- inferType env listExpr
    case listType of
        TList _ -> Right TInt
        _ -> Left "length requires a list argument"
inferType env (ListAppend listExpr elemExpr) = do
    listType <- inferType env listExpr
    elemType <- inferType env elemExpr
    case listType of
        TList expectedElemType ->
            if elemType == expectedElemType
                then Right (TList elemType)
                else Left $ "Cannot append " ++ show elemType ++ " to list of " ++ show expectedElemType
        _ -> Left "append requires a list as first argument"
inferType env (ListNth listExpr indexExpr) = do
    listType <- inferType env listExpr
    indexType <- inferType env indexExpr
    case (listType, indexType) of
        (TList elemType, TInt) -> Right elemType
        (TList _, _) -> Left "List index must be an integer"
        (_, TInt) -> Left "nth requires a list as first argument"
        _ -> Left "nth requires a list and integer arguments"
inferType env (ListReverse listExpr) = do
    listType <- inferType env listExpr
    case listType of
        TList elemType -> Right (TList elemType)
        _ -> Left "reverse requires a list argument"
inferType env (ListElem elemExpr listExpr) = do
    elemType <- inferType env elemExpr
    listType <- inferType env listExpr
    case listType of
        TList expectedElemType ->
            if elemType == expectedElemType
                then Right TBool
                else Left $ "Cannot check if " ++ show elemType ++ " is in list of " ++ show expectedElemType
        _ -> Left "elem requires a list as second argument"
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