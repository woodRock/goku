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
                if isBinaryOperator t || isNewStatement ts
                then Right (currentFunc, ts)
                else case runParser' parsePrimary ts of
                    Right (arg, ts') -> runParser' (parseArgs (App currentFunc arg)) ts'
                    Left _ -> Right (currentFunc, ts)
            Left _ -> Right (currentFunc, ts) -- End of input, stop parsing arguments

-- Check if the current token stream starts a new statement
isNewStatement :: [Token] -> Bool
isNewStatement (TVar _ : TAssign : _) = True  -- Variable assignment
isNewStatement (TLet : _) = True
isNewStatement (TSet : _) = True
isNewStatement (TIf : _) = True
isNewStatement (TWhile : _) = True
isNewStatement (TReturn : _) = True
isNewStatement (TAssert : _) = True
isNewStatement [TEOF] = True
isNewStatement [] = True
isNewStatement _ = False

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
            -- Check if this is a parameter list for a lambda or just a parenthesized expression
            tokens <- getTokens
            case tokens of
                (LParen : TVar _ : _) -> parseLambdaWithParams
                (LParen : RParen : _) -> parseLambdaWithParams -- No params
                _ -> do
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
                    return $ Lam [s] TInt e
                _ -> parseError "Expected a variable after lambda"
        _ -> parseError "Expected a primary expression"

parseLambdaWithParams :: Parser Expr
parseLambdaWithParams = do
    consume LParen
    params <- parseParamList
    consume RParen
    consume TArrow
    body <- parseExprOrBlock
    return $ Lam params TInt body

parseParamList :: Parser [String]
parseParamList = do
    t <- peek
    case t of
        RParen -> return [] -- No parameters
        TVar name -> do
            consume t
            rest <- parseRestParams
            return (name : rest)
        _ -> parseError "Expected parameter name or closing parenthesis"

parseRestParams :: Parser [String]
parseRestParams = do
    t <- peek
    case t of
        TComma -> do
            consume TComma
            name <- parseVar
            rest <- parseRestParams
            return (name : rest)
        _ -> return []

parseExprOrBlock :: Parser Expr
parseExprOrBlock = do
    t <- peek
    case t of
        LBrace -> do
            stmts <- parseBlock
            -- For function bodies, we need to find the return statement
            case findReturnExpr stmts of
                Just expr -> return expr
                Nothing -> parseError "Function body must return a value"
        _ -> parseExpr

-- Helper function to extract the expression from a return statement in a block
findReturnExpr :: [Stmt] -> Maybe Expr
findReturnExpr [] = Nothing
findReturnExpr [Return expr] = Just expr
findReturnExpr (Return expr : _) = Just expr
findReturnExpr (_ : rest) = findReturnExpr rest

-- This is the fixed statement parser.
parseStmt :: Parser Stmt
parseStmt = do
    t <- peek
    case t of
        LBrace -> Block <$> parseBlock
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
                -- Check for function definition: name = (params) -> expr or block
                (_ : TAssign : LParen : _) -> parseFunctionDef
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

parseFunctionDef :: Parser Stmt
parseFunctionDef = do
    name <- parseVar
    consume TAssign
    funcExpr <- parseLambdaWithParams
    return $ Set name funcExpr

parseBlock :: Parser [Stmt]
parseBlock = do
    consume LBrace
    stmts <- many parseStmt
    consume RBrace
    return stmts

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
