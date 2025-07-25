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
  | TEquals
  | TPlus
  | TMinus
  | TMult
  | TDiv
  | TIntDiv
  | TIf
  | TThen
  | TElse
  | TWhile
  | TDo
  | TReturn
  | TAssert
  | TSet
  | LParen
  | RParen
  | LBrace
  | RBrace
  | TComma
  | TLessThan
  | TEOF
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = [TEOF]
tokenize ('-':'>':cs) = TArrow : tokenize cs
tokenize ('=':'=':cs) = TEquals : tokenize cs
tokenize ('/':'/':cs) = TIntDiv : tokenize cs
tokenize ('#':cs) = tokenize (dropWhile (/= '\n') cs) -- Skip comments
tokenize (c:cs)
    
    | c `elem` " \t\n" = tokenize cs
    | isAlpha c =
        let (var, rest) = span isAlphaNum (c:cs)
        in keyword var : tokenize rest
    | isDigit c =
        let (num, rest) = span isDigit (c:cs)
        in TLitInt (read num) : tokenize rest
    | c == '=' = case cs of
        ('=':rest) -> TEquals : tokenize rest
        _ -> TAssign : tokenize cs
    | c == '+' = TPlus : tokenize cs
    | c == '-' = TMinus : tokenize cs
    | c == '*' = TMult : tokenize cs
    | c == '/' = TDiv : tokenize cs
    | c == '<' = TLessThan : tokenize cs
    | c == '\\' = TLam : tokenize cs
    | c == '(' = LParen : tokenize cs
    | c == ')' = RParen : tokenize cs
    | c == '{' = LBrace : tokenize cs
    | c == '}' = RBrace : tokenize cs
    | c == ',' = TComma : tokenize cs
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
keyword "set" = TSet
keyword s = TVar s