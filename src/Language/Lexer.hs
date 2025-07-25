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
  | TLitString String
  | TLet
  | TAssign
  | TEquals
  | TPlus
  | TMinus
  | TMult
  | TDiv
  | TIntDiv
  | TConcat
  | TIf
  | TThen
  | TElse
  | TWhile
  | TDo
  | TReturn
  | TAssert
  | TPrint
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
tokenize ('"':cs) = let (str, rest) = parseString cs in TLitString str : tokenize rest
tokenize ('+':'+':cs) = TConcat : tokenize cs
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
keyword "print" = TPrint
keyword "set" = TSet
keyword s = TVar s

-- Parse a string literal until the closing quote
parseString :: [Char] -> (String, [Char])
parseString [] = error "Unterminated string literal"
parseString ('"':cs) = ("", cs)  -- Empty string or end of string
parseString ('\\':c:cs) = 
  let (str, rest) = parseString cs
  in (escapeChar c : str, rest)
parseString (c:cs) = 
  let (str, rest) = parseString cs
  in (c : str, rest)

-- Handle escape sequences
escapeChar :: Char -> Char
escapeChar 'n' = '\n'
escapeChar 't' = '\t'
escapeChar 'r' = '\r'
escapeChar '\\' = '\\'
escapeChar '"' = '"'
escapeChar c = c  -- For any other character, just return it as-is