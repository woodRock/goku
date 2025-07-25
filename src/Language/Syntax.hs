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
  | Lam [String] Type Expr -- Support multiple parameters
  | App Expr Expr
  | LitInt Int
  | LitBool Bool
  | Equals Expr Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | IntDiv Expr Expr
  | LessThan Expr Expr
  | IfExpr Expr Expr Expr -- Conditional expression: if condition then expr1 else expr2
  deriving (Show, Eq)

data Stmt
  = ExprStmt Expr
  | Let String Type Expr
  | Set String Expr
  | If Expr Stmt Stmt
  | While Expr Stmt
  | Return Expr
  | Assert Expr
  | Block [Stmt] -- Support block statements
  deriving (Show, Eq)   

data Program = Program [Stmt]
  deriving (Show, Eq)
  