-- Define a simple syntax tree for a language of 
-- lambda calculus expresssions with strong typing.

module Language.Syntax
    ( Type(..)
    , Expr(..)
    , Stmt(..)
    , Program(..)
    ) where

data Type = TInt | TBool | TString | TList Type | TFun Type Type
  deriving (Show, Eq)

data Expr
  = Var String
  | Lam [String] Type Expr -- Support multiple parameters
  | App Expr Expr
  | LitInt Int
  | LitBool Bool
  | LitString String
  | LitList [Expr] -- List literal: [1, 2, 3]
  | ListCons Expr Expr -- Cons operation: x :: xs
  | ListHead Expr -- Head operation: head(list)
  | ListTail Expr -- Tail operation: tail(list)
  | ListEmpty Expr -- Check if list is empty: empty(list)
  | ListLength Expr -- Get list length: length(list)
  | ListAppend Expr Expr -- Append element to end: append(list, elem)
  | ListNth Expr Expr -- Get element at index: nth(list, index)
  | ListReverse Expr -- Reverse list: reverse(list)
  | ListElem Expr Expr -- Check membership: elem(elem, list)
  | Equals Expr Expr
  | NotEquals Expr Expr
  | LessThan Expr Expr
  | LessThanEqual Expr Expr
  | GreaterThan Expr Expr
  | GreaterThanEqual Expr Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | IntDiv Expr Expr
  | Mod Expr Expr -- Modulo operator
  | Concat Expr Expr -- String concatenation
  | IfExpr Expr Expr Expr -- Conditional expression: if condition then expr1 else expr2
  deriving (Show, Eq)

data Stmt
  = ExprStmt Expr
  | Let String Type Expr
  | Set String Expr
  | If Expr Stmt (Maybe Stmt)  -- Made else branch optional
  | While Expr Stmt
  | Return Expr
  | Assert Expr
  | Print Expr -- Print statement
  | Block [Stmt] -- Support block statements
  deriving (Show, Eq)   

data Program = Program [Stmt]
  deriving (Show, Eq)
  