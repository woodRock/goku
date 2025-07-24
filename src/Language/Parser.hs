module Language.Parser
    ( Parser
    , ParseError(..)
    , runParser
    , parseExpr
    , parseStmt
    , parseProgram
    ) where

import Language.Syntax
import Language.Lexer

newtype Parser a = Parser { runParser' :: [Token] -> Either ParseError (a, [Token]) }

data ParseError = ParseError String deriving (Show, Eq)

instance Functor Parser where
    fmap f (Parser p) = Parser $ \ts -> do
        (a, ts') <- p ts
        Right (f a, ts')

instance Applicative Parser where
    pure a = Parser $ \ts -> Right (a, ts)
    (Parser p1) <*> (Parser p2) = Parser $ \ts -> do
        (f, ts') <- p1 ts
        (a, ts'') <- p2 ts'
        Right (f a, ts'')

instance Monad Parser where
    (Parser p) >>= f = Parser $ \ts -> do
        (a, ts') <- p ts
        runParser' (f a) ts'

runParser :: Parser a -> String -> Either ParseError a
runParser p s = fst <$> runParser' p (tokenize s)

parseError :: String -> Parser a
parseError msg = Parser $ \_ -> Left $ ParseError msg

peek :: Parser Token
peek = Parser $ \ts -> case ts of
    [] -> Left $ ParseError "Unexpected end of input"
    (t:_) -> Right (t, ts)

consume :: Token -> Parser ()
consume t = Parser $ \ts -> case ts of
    (t':ts') | t == t' -> Right ((), ts')
    _ -> Left $ ParseError $ "Expected " ++ show t

parseExpr :: Parser Expr
parseExpr = parseAtom >>= parseApp

parseAtom :: Parser Expr
parseAtom = do
    t <- peek
    case t of
        TLitInt n -> do
            consume t
            return $ LitInt n
        TLitBool b -> do
            consume t
            return $ LitBool b
        TVar s -> do
            consume t
            return $ Var s
        LParen -> do
            consume LParen
            e <- parseExpr
            consume RParen
            return e
        TLam -> do
            consume TLam
            t' <- peek
            case t' of 
                TVar s -> do
                    consume t'
                    consume TArrow
                    e <- parseExpr
                    return $ Lam s TInt e
                _ -> parseError "Expected a variable after lambda"
        _ -> parseError "Expected an expression"

parseApp :: Expr -> Parser Expr
parseApp e = do
    t <- peek
    case t of
        TVar _ -> do
            e' <- parseAtom
            parseApp (App e e')
        TLitInt _ -> do
            e' <- parseAtom
            parseApp (App e e')
        TLitBool _ -> do
            e' <- parseAtom
            parseApp (App e e')
        LParen -> do
            e' <- parseAtom
            parseApp (App e e')
        _ -> return e

parseStmt :: Parser Stmt
parseStmt = do
    t <- peek
    case t of
        TLet -> do
            consume TLet
            name <- parseVar
            consume TAssign
            expr <- parseExpr
            return $ Let name TInt expr -- Assuming TInt for now
        TIf -> do
            consume TIf
            cond <- parseExpr
            consume TThen
            thenBranch <- parseStmt
            consume TElse
            elseBranch <- parseStmt
            return $ If cond thenBranch elseBranch
        TWhile -> do
            consume TWhile
            cond <- parseExpr
            consume TDo
            body <- parseStmt
            return $ While cond body
        TReturn -> do
            consume TReturn
            expr <- parseExpr
            return $ Return expr
        TAssert -> do
            consume TAssert
            expr <- parseExpr
            return $ Assert expr
        _ -> do
            expr <- parseExpr
            return $ ExprStmt expr

parseVar :: Parser String
parseVar = do
    t <- peek
    case t of
        TVar s -> do
            consume t
            return s
        _ -> parseError "Expected a variable name"

parseProgram :: Parser Program
parseProgram = Program <$> many parseStmt

many :: Parser a -> Parser [a]
many p = (do
    x <- p
    xs <- many p
    return (x:xs))
    <|> return []

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser $ \ts -> case runParser' p1 ts of
    Right (a, ts') -> Right (a, ts')
    Left _ -> runParser' p2 ts