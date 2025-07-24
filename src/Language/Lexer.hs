module Language.Lexer
    ( Token(..)
    , tokenize
    ) where

import Data.Char (isAlpha, isDigit, isAlphaNum)

data Token
  = TVar String
  | TLam
  | TArrow
  | TLitInt Int
  | TLitBool Bool
  | TLet
  | TAssign
  | TIf
  | TThen
  | TElse
  | TWhile
  | TDo
  | TReturn
  | TAssert
  | LParen
  | RParen
  | TEOF
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = [TEOF]
tokenize ('-':'>':cs) = TArrow : tokenize cs
tokenize (c:cs)
    | c `elem` " \t\n" = tokenize cs
    | isAlpha c =
        let (var, rest) = span isAlphaNum (c:cs)
        in keyword var : tokenize rest
    | isDigit c =
        let (num, rest) = span isDigit (c:cs)
        in TLitInt (read num) : tokenize rest
    | c == '=' = TAssign : tokenize cs
    | c == '\\' = TLam : tokenize cs
    | c == '(' = LParen : tokenize cs
    | c == ')' = RParen : tokenize cs
    | otherwise = tokenize cs -- Skip unknown characters

keyword :: String -> Token
keyword "let" = TLet
keyword "if" = TIf
keyword "then" = TThen
keyword "else" = TElse
keyword "while" = TWhile
keyword "do" = TDo
keyword "return" = TReturn
keyword "true" = TLitBool True
keyword "false" = TLitBool False
keyword "assert" = TAssert
keyword s = TVar s
