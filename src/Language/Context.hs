module Language.Context
    ( Context
    ) where

import Language.Syntax

type Context = [(String, Expr)]