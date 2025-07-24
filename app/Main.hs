{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import System.Environment (getArgs)
import System.IO (readFile)
import Language.Parser (runParser, parseProgram)
import Language.Evaluator (evalProgram, GokuError(..))
import Control.Exception (catch, SomeException, fromException)
import System.Exit (exitFailure)

main :: IO ()
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
              _ <- evalProgram program [] -- Evaluate the program
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
    _ -> do
      putStrLn "Usage: goku <file>"
      exitFailure