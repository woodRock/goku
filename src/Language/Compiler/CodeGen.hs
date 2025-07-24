module Language.Compiler.CodeGen
    ( generateC
    ) where

import Language.Syntax

-- | Generates C code from a Goku program.
generateC :: Program -> String
generateC (Program stmts) =
    "#include <stdio.h>\n" ++
    "#include <stdbool.h>\n" ++
    "#include <stdlib.h>\n\n" ++
    "void assert_goku(bool condition, const char* message);\n\n" ++
    "int main() {\n" ++
    "    // Context for variables (simple for now, could be a hash map)\n" ++
    "    // For simplicity, all variables are int for now.\n" ++
    "    // In a real compiler, types would be tracked.\n" ++
    unlines (map (indent 4 . genStmt) stmts) ++
    "    return 0;\n" ++
    "}\n\n" ++
    "// Helper for assertions\n" ++
    "void assert_goku(bool condition, const char* message) {\n" ++
    "    if (!condition) {\n" ++
    "        fprintf(stderr, \"Assertion failed! %s\\n\", message);\n" ++
    "        exit(1);\n" ++
    "    }\n" ++
    "}\n"

-- | Generates C code for a single Goku statement.
genStmt :: Stmt -> String
genStmt (ExprStmt expr) = genExpr expr ++ ";\n"
genStmt (Let name _ expr) = "int " ++ name ++ " = " ++ genExpr expr ++ ";\n"
genStmt (Set name expr) = name ++ " = " ++ genExpr expr ++ ";\n"
genStmt (If cond thenBranch elseBranch) =
    "if (" ++ genExpr cond ++ ") {\n" ++
    indent 4 (genStmt thenBranch) ++
    "} else {\n" ++
    indent 4 (genStmt elseBranch) ++
    "}\n"
genStmt (While cond body) =
    "while (" ++ genExpr cond ++ ") {\n" ++
    indent 4 (genStmt body) ++
    "}\n"
genStmt (Return expr) = genExpr expr ++ ";\n"
genStmt (Assert expr) = "assert_goku(" ++ (if isUnsupportedExpr expr then "false" else genExpr expr) ++ ", \"" ++ escapeCStr (genExpr expr) ++ "\");\n"

-- | Escapes double quotes in a string for C string literals.
escapeCStr :: String -> String
escapeCStr = concatMap (\c -> if c == '"' then "\"" else [c])

isUnsupportedExpr :: Expr -> Bool
isUnsupportedExpr (Lam _ _ _) = True
isUnsupportedExpr (App _ _) = True
isUnsupportedExpr _ = False


-- | Generates C code for a single Goku expression.
genExpr :: Expr -> String
genExpr (LitInt n) = show n
genExpr (LitBool b) = if b then "true" else "false"
genExpr (Var name) = name
genExpr (Add e1 e2) = "(" ++ genExpr e1 ++ " + " ++ genExpr e2 ++ ")"
genExpr (Equals e1 e2) = "(" ++ genExpr e1 ++ " == " ++ genExpr e2 ++ ")"
genExpr (LessThan e1 e2) = "(" ++ genExpr e1 ++ " < " ++ genExpr e2 ++ ")"
-- Lambdas and applications are more complex and would require
-- function pointer support or a different compilation strategy (e.g., closure conversion).
-- For this basic example, we'll omit direct C generation for them.
genExpr (Lam _ _ _) = "/* Lambda expression - not directly supported in this basic C codegen */"
genExpr (App _ _) = "/* Function application - not directly supported in this basic C codegen */"

-- | Indents a string by a given number of spaces.
indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines