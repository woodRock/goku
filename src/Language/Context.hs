module Language.Context
    ( Context
    , emptyContext
    ) where

import Language.Syntax

type Context = [(String, Expr)]

emptyContext :: Context
emptyContext = []