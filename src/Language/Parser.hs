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

-- This helper parser allows us to look at the current token stream without consuming it.
-- We'll use it to look ahead for an assignment.
getTokens :: Parser [Token]
getTokens = Parser $ \ts -> Right (ts, ts)

consume :: Token -> Parser ()
consume t = Parser $ \ts -> case ts of
    (t':ts') | t == t' -> Right ((), ts')
    _ -> Left $ ParseError $ "Expected " ++ show t

parseExpr :: Parser Expr
parseExpr = parseEquality

parseEquality :: Parser Expr
parseEquality = parseBinaryOp parseLessThan TEquals Equals

parseLessThan :: Parser Expr
parseLessThan = parseBinaryOp parseAddition TLessThan LessThan

parseAddition :: Parser Expr
parseAddition = parseBinaryOp parseApplication TPlus Add

parseApplication :: Parser Expr
parseApplication = do
    func <- parsePrimary
    parseArgs func
  where
    parseArgs currentFunc = Parser $ \ts ->
        case runParser' peek ts of
            Right (t, _) ->
                if isBinaryOperator t
                then Right (currentFunc, ts)
                else case runParser' parsePrimary ts of
                    Right (arg, ts') -> runParser' (parseArgs (App currentFunc arg)) ts'
                    Left _ -> Right (currentFunc, ts)
            Left _ -> Right (currentFunc, ts) -- End of input, stop parsing arguments

isBinaryOperator :: Token -> Bool
isBinaryOperator TEquals = True
isBinaryOperator TLessThan = True
isBinaryOperator TPlus = True
isBinaryOperator _ = False

parseBinaryOp :: Parser Expr -> Token -> (Expr -> Expr -> Expr) -> Parser Expr
parseBinaryOp nextParser opToken constructor = do
    left <- nextParser
    tryParseOp left
  where
    tryParseOp left = do
        t <- peek
        case t of
            token | token == opToken -> do
                consume opToken
                right <- nextParser
                tryParseOp (constructor left right)
            _ -> return left

parsePrimary :: Parser Expr
parsePrimary = do
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
        _ -> parseError "Expected a primary expression"

-- This is the fixed statement parser.
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
        TSet -> do
            consume TSet
            name <- parseVar
            consume TAssign
            expr <- parseExpr
            return $ Set name expr
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
        -- If the statement starts with a variable, we need to disambiguate.
        TVar _ -> do
            tokens <- getTokens
            case tokens of
                -- If the variable is followed by '=', it's an assignment.
                (_ : TAssign : _) -> do
                    name <- parseVar
                    consume TAssign
                    expr <- parseExpr
                    return $ Set name expr
                -- Otherwise, it's an expression statement (e.g., a function call).
                _ -> ExprStmt <$> parseExpr
        -- For any other token, it must be the start of an expression statement.
        _ -> ExprStmt <$> parseExpr

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
many p = Parser $ \ts -> case runParser' p ts of
    Right (x, ts') -> case runParser' (many p) ts' of
        Right (xs, ts'') -> Right (x:xs, ts'')
        Left err -> Left err
    Left _ -> Right ([], ts)

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser $ \ts -> case runParser' p1 ts of
    Right (a, ts') -> Right (a, ts')
    Left _ -> runParser' p2 ts
