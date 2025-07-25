{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import System.Environment (getArgs)
import System.IO (readFile, writeFile, hFlush, stdout)
import Language.Parser (runParser, parseProgram, parseExpr, parseStmt)
import Language.Evaluator (GokuError(..), evalProgram, eval, evalStmt)
import Language.Context (emptyContext, Context)
import Language.Compiler.Optimizer (optimizeProgram)
import Language.Compiler.CodeGen (generateC)
import Language.Syntax (Stmt(..), Expr(..))
import Control.Exception (catch, SomeException, fromException)
import System.Exit (exitFailure, ExitCode(ExitSuccess, ExitFailure))
import System.Process (readProcessWithExitCode)
import System.Directory (removeFile)
import System.FilePath (takeBaseName, takeDirectory)

-- Main entry point for the Goku interpreter
-- It reads a file containing Goku code, parses it, and either interprets or compiles the program.
main = do
  args <- getArgs
  case args of
    ["--interpret", filePath] -> interpretFile filePath
    ["--compile", filePath] -> compileFile filePath
    ["--interactive"] -> interactiveMode
    ["--repl"] -> interactiveMode  -- Alias for interactive mode
    [filePath] -> compileFile filePath  -- Default to compilation for backward compatibility
    [] -> interactiveMode  -- Default to interactive mode when no arguments
    _ -> do
      putStrLn "Usage: goku [--interpret|--compile|--interactive] <file>"
      putStrLn "       goku  (starts interactive mode)"
      putStrLn "  --interpret     : Run code directly using the evaluator"
      putStrLn "  --compile       : Compile to C and execute"
      putStrLn "  --interactive   : Start interactive REPL mode"
      putStrLn "  --repl          : Alias for --interactive"
      exitFailure

-- Interpret a Goku file directly using the evaluator
interpretFile :: String -> IO ()
interpretFile filePath = do
  contents <- readFile filePath
  case runParser parseProgram contents of
    Left err -> do
      putStrLn $ "Parse Error: " ++ show err
      exitFailure
    Right program -> do
      catch (
        do
          let optimizedProgram = optimizeProgram program
          putStrLn $ "Interpreting " ++ filePath ++ "..."
          _ <- evalProgram optimizedProgram emptyContext
          putStrLn "Program executed successfully."
        ) (
        \e -> case fromException e of
          Just (gokuError :: GokuError) -> do
            putStrLn $ "Runtime Error: " ++ show gokuError
            exitFailure
          Nothing -> do
            putStrLn $ "Unexpected Error: " ++ show (e :: SomeException)
            exitFailure
        )

-- Compile a Goku file to C and execute
compileFile :: String -> IO ()
compileFile filePath = do
  contents <- readFile filePath
  case runParser parseProgram contents of
    Left err -> do
      putStrLn $ "Parse Error: " ++ show err
      exitFailure
    Right program -> do
      catch (
        do
          let optimizedProgram = optimizeProgram program
          let cCode = generateC optimizedProgram
          let fileDir = takeDirectory filePath
          let baseName = takeBaseName filePath
          let cFileName = fileDir ++ "/" ++ baseName ++ ".c"
          let executableName = fileDir ++ "/" ++ baseName ++ ".out"

          writeFile cFileName cCode
          putStrLn $ "Generated C code to " ++ cFileName

          -- Compile the C code
          (exitCode, stdout, stderr) <- readProcessWithExitCode "gcc" [cFileName, "-o", executableName] ""
          case exitCode of
            ExitSuccess -> do
              putStrLn $ "Compiled C code to " ++ executableName
              -- Run the compiled executable
              (runExitCode, runStdout, runStderr) <- readProcessWithExitCode ("./" ++ executableName) [] ""
              case runExitCode of
                ExitSuccess -> do
                  putStrLn "Program executed successfully."
                  putStrLn runStdout
                ExitFailure _ -> do
                  putStrLn $ "Program execution failed: " ++ runStderr
                  exitFailure
            ExitFailure _ -> do
              putStrLn $ "C compilation failed: " ++ stderr
              exitFailure

          -- Clean up generated files
          removeFile cFileName
          removeFile executableName
        ) (
        \e -> case fromException e of
          Just (gokuError :: GokuError) -> do
            putStrLn $ "Runtime Error: " ++ show gokuError
            exitFailure
          Nothing -> do
            putStrLn $ "Unexpected Error: " ++ show (e :: SomeException)
            exitFailure
        )

-- Interactive REPL mode
interactiveMode :: IO ()
interactiveMode = do
  putStrLn "Welcome to Goku Interactive Mode (REPL)"
  putStrLn "Type expressions or statements to evaluate them."
  putStrLn "Type ':quit' or ':q' to exit."
  putStrLn "Type ':help' or ':h' for help."
  putStrLn ""
  replLoop emptyContext

-- Main REPL loop
replLoop :: Context -> IO ()
replLoop ctx = do
  putStr "goku> "
  hFlush stdout
  input <- getLine
  case input of
    ":quit" -> putStrLn "Goodbye!"
    ":q" -> putStrLn "Goodbye!"
    ":help" -> do
      putStrLn "Available commands:"
      putStrLn "  :quit, :q    - Exit the REPL"
      putStrLn "  :help, :h    - Show this help message"
      putStrLn "  :ctx         - Show current context (variables)"
      putStrLn "  :clear       - Clear all variables"
      putStrLn ""
      putStrLn "You can enter expressions (like 2 + 3) or statements (like x = 5)."
      putStrLn "Function definitions: add = (x,y) -> x + y"
      putStrLn "Lambda expressions: (x) -> x * 2"
      putStrLn "Assertions: assert (x == 5)"
      replLoop ctx
    ":h" -> do
      putStrLn "Available commands:"
      putStrLn "  :quit, :q    - Exit the REPL"
      putStrLn "  :help, :h    - Show this help message"
      putStrLn "  :ctx         - Show current context (variables)"
      putStrLn "  :clear       - Clear all variables"
      replLoop ctx
    ":ctx" -> do
      if null ctx
        then putStrLn "No variables defined."
        else do
          putStrLn "Current variables:"
          mapM_ (\(name, _) -> putStrLn $ "  " ++ name) ctx
      replLoop ctx
    ":clear" -> do
      putStrLn "Context cleared."
      replLoop emptyContext
    "" -> replLoop ctx  -- Empty input, just continue
    _ -> do
      newCtx <- processInput input ctx
      replLoop newCtx

-- Process user input (expression or statement)
processInput :: String -> Context -> IO Context
processInput input ctx = catch (
  do
    -- First try to parse as a statement
    case runParser parseStmt input of
      Right stmt -> do
        newCtx <- evalStmt stmt ctx
        case stmt of
          ExprStmt expr -> do
            (result, _) <- eval expr ctx
            putStrLn $ "=> " ++ show result
          _ -> putStrLn "Statement executed."
        return newCtx
      Left _ -> do
        -- If statement parsing fails, try as expression
        case runParser parseExpr input of
          Right expr -> do
            (result, newCtx) <- eval expr ctx
            putStrLn $ "=> " ++ show result
            return newCtx
          Left err -> do
            putStrLn $ "Parse Error: " ++ show err
            return ctx
  ) (
  \e -> case fromException e of
    Just (gokuError :: GokuError) -> do
      putStrLn $ "Error: " ++ show gokuError
      return ctx
    Nothing -> do
      putStrLn $ "Unexpected Error: " ++ show (e :: SomeException)
      return ctx
  )