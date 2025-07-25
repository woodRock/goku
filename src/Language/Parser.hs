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
parseLessThan = parseBinaryOp parseAdditionAndSubtraction TLessThan LessThan

parseAdditionAndSubtraction :: Parser Expr
parseAdditionAndSubtraction = do
    left <- parseMultiplicationAndDivision
    parseRest left
  where
    parseRest left = do
        t <- peek
        case t of
            TPlus -> do
                consume TPlus
                right <- parseMultiplicationAndDivision
                parseRest (Add left right)
            TMinus -> do
                consume TMinus
                right <- parseMultiplicationAndDivision
                parseRest (Sub left right)
            _ -> return left

parseMultiplicationAndDivision :: Parser Expr
parseMultiplicationAndDivision = do
    left <- parseApplication
    parseRest left
  where
    parseRest left = do
        t <- peek
        case t of
            TMult -> do
                consume TMult
                right <- parseApplication
                parseRest (Mult left right)
            TDiv -> do
                consume TDiv
                right <- parseApplication
                parseRest (Div left right)
            TIntDiv -> do
                consume TIntDiv
                right <- parseApplication
                parseRest (IntDiv left right)
            _ -> return left

parseApplication :: Parser Expr
parseApplication = do
    func <- parsePrimary
    parseArgs func
  where
    parseArgs currentFunc = Parser $ \ts ->
        case runParser' peek ts of
            Right (LParen, _) -> 
                -- Handle parenthesized function calls like f(x, y)
                case runParser' parseParenthesizedCall ts of
                    Right (args, ts') -> Right (foldl App currentFunc args, ts')
                    Left _ -> Right (currentFunc, ts)
            Right (t, _) ->
                if isBinaryOperator t || isNewStatement ts
                then Right (currentFunc, ts)
                else case runParser' parsePrimary ts of
                    Right (arg, ts') -> runParser' (parseArgs (App currentFunc arg)) ts'
                    Left _ -> Right (currentFunc, ts)
            Left _ -> Right (currentFunc, ts) -- End of input, stop parsing arguments

-- Parse parenthesized function call arguments like (x, y, z)
parseParenthesizedCall :: Parser [Expr]
parseParenthesizedCall = do
    consume LParen
    args <- parseArgList
    consume RParen
    return args

-- Parse comma-separated argument list
parseArgList :: Parser [Expr]
parseArgList = do
    t <- peek
    case t of
        RParen -> return [] -- No arguments
        _ -> do
            first <- parseExpr
            rest <- parseRestArgs
            return (first : rest)
  where
    parseRestArgs = Parser $ \ts ->
        case runParser' peek ts of
            Right (TComma, _) -> do
                case runParser' (consume TComma >> parseExpr) ts of
                    Right (arg, ts') -> 
                        case runParser' parseRestArgs ts' of
                            Right (args, ts'') -> Right (arg : args, ts'')
                            Left _ -> Right ([arg], ts')
                    Left err -> Left err
            _ -> Right ([], ts)

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
isBinaryOperator TMinus = True
isBinaryOperator TMult = True
isBinaryOperator TDiv = True
isBinaryOperator TIntDiv = True
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
                (LParen : TVar _ : RParen : TArrow : _) -> parseLambdaWithParams
                (LParen : TVar _ : TComma : _) -> parseLambdaWithParams  -- Multi-param lambda
                (LParen : RParen : TArrow : _) -> parseLambdaWithParams -- No params lambda
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
        TIf -> do
            consume TIf
            cond <- parseExpr
            consume TThen
            thenExpr <- parseExpr
            consume TElse
            elseExpr <- parseExpr
            return $ IfExpr cond thenExpr elseExpr
        _ -> parseError "Expected a primary expression"

parseLambdaWithParams :: Parser Expr
parseLambdaWithParams = do
    consume LParen
    params <- parseParamList
    consume RParen
    consume TArrow
    body <- parseLambdaBody
    return $ Lam params TInt body

-- Parse lambda body which can be either an expression or a block
parseLambdaBody :: Parser Expr
parseLambdaBody = do
    t <- peek
    case t of
        LBrace -> do
            stmts <- parseBlock
            case stmts of
                [Return expr] -> return expr
                [If cond (Return thenExpr) (Return elseExpr)] -> 
                    return $ IfExpr cond thenExpr elseExpr
                [If cond thenStmt elseStmt] -> do
                    -- Convert statement-based if to expression-based if
                    thenExpr <- stmtToExpr thenStmt
                    elseExpr <- stmtToExpr elseStmt
                    return $ IfExpr cond thenExpr elseExpr
                [stmt] -> do
                    -- Handle any single statement that can be converted to expression
                    stmtToExpr stmt
                _ -> parseError "Lambda body must be a single statement that can be converted to an expression"
        _ -> parseExpr

-- Helper to convert simple statements to expressions
stmtToExpr :: Stmt -> Parser Expr
stmtToExpr (Return expr) = return expr
stmtToExpr (Block [Return expr]) = return expr
stmtToExpr (Block [If cond (Return thenExpr) (Return elseExpr)]) = 
    return $ IfExpr cond thenExpr elseExpr
stmtToExpr (Block [If cond thenStmt elseStmt]) = do
    -- Handle nested if with block statements
    thenExpr <- stmtToExpr thenStmt
    elseExpr <- stmtToExpr elseStmt
    return $ IfExpr cond thenExpr elseExpr
stmtToExpr (If cond thenStmt elseStmt) = do
    -- Handle if statements directly
    thenExpr <- stmtToExpr thenStmt
    elseExpr <- stmtToExpr elseStmt
    return $ IfExpr cond thenExpr elseExpr
stmtToExpr _ = parseError "Cannot convert complex statement to expression"

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
