-- Define a simple syntax tree for a language of 
-- lambda calculus expresssions with strong typing.

module Language.Syntax
    ( Type(..)
    , Expr(..)
    , Stmt(..)
    , Program(..)
    ) where

data Type = TInt | TBool | TFun Type Type
  deriving (Show, Eq)

data Expr
  = Var String
  | Lam String Type Expr
  | App Expr Expr
  | LitInt Int
  | LitBool Bool
  deriving (Show, Eq)

data Stmt
  = ExprStmt Expr
  | Let String Type Expr
  | If Expr Stmt Stmt
  | While Expr Stmt
  | Return Expr
  | Assert Expr
  deriving (Show, Eq)   

data Program = Program [Stmt]
  deriving (Show, Eq)