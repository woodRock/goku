{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import System.Environment (getArgs)
import System.IO (readFile, writeFile)
import Language.Parser (runParser, parseProgram)
import Language.Evaluator (GokuError(..))
import Language.Compiler.Optimizer (optimizeProgram)
import Language.Compiler.CodeGen (generateC)
import Control.Exception (catch, SomeException, fromException)
import System.Exit (exitFailure, ExitCode(ExitSuccess, ExitFailure))
import System.Process (readProcessWithExitCode)
import System.Directory (removeFile)
import System.FilePath (takeBaseName, takeDirectory)

-- Main entry point for the Goku interpreter
-- It reads a file containing Goku code, parses it, and evaluates the program.
main = do
  args <- getArgs
  case args of
    [filePath] -> do
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
    _ -> do
      putStrLn "Usage: goku <file>"
      exitFailure