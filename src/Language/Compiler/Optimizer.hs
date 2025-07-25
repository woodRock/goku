module Language.Compiler.Optimizer
    ( optimizeProgram
    ) where

import Language.Syntax

-- | Applies optimization passes to a Goku program.
optimizeProgram :: Program -> Program
optimizeProgram (Program stmts) = Program (map optimizeStmt stmts)

-- | Optimizes a single statement.
optimizeStmt :: Stmt -> Stmt
optimizeStmt (ExprStmt expr) = ExprStmt (optimizeExpr expr)
optimizeStmt (Let name t expr) = Let name t (optimizeExpr expr)
optimizeStmt (Set name expr) = Set name (optimizeExpr expr)
optimizeStmt (If cond thenBranch maybeElseBranch) =
    If (optimizeExpr cond) (optimizeStmt thenBranch) (fmap optimizeStmt maybeElseBranch)
optimizeStmt (While cond body) =
    While (optimizeExpr cond) (optimizeStmt body)
optimizeStmt (Return expr) = Return (optimizeExpr expr)
optimizeStmt (Assert expr) = Assert (optimizeExpr expr)
optimizeStmt (Print expr) = Print (optimizeExpr expr)
optimizeStmt (Block stmts) = Block (map optimizeStmt stmts)

-- | Optimizes a single expression (e.g., constant folding).
optimizeExpr :: Expr -> Expr
optimizeExpr (Add e1 e2) =
    case (optimizeExpr e1, optimizeExpr e2) of
        (LitInt n1, LitInt n2) -> LitInt (n1 + n2)
        (optE1, optE2) -> Add optE1 optE2
optimizeExpr (Equals e1 e2) =
    case (optimizeExpr e1, optimizeExpr e2) of
        (LitInt n1, LitInt n2) -> LitBool (n1 == n2)
        (LitBool b1, LitBool b2) -> LitBool (b1 == b2)
        (optE1, optE2) -> Equals optE1 optE2
optimizeExpr (LessThan e1 e2) =
    case (optimizeExpr e1, optimizeExpr e2) of
        (LitInt n1, LitInt n2) -> LitBool (n1 < n2)
        (optE1, optE2) -> LessThan optE1 optE2
optimizeExpr (App func arg) = App (optimizeExpr func) (optimizeExpr arg)
optimizeExpr (Lam name t body) = Lam name t (optimizeExpr body)
optimizeExpr other = other -- For LitInt, LitBool, Var, etc., no optimization