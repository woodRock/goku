{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Language.Syntax
import Language.Parser (parseExpr, parseStmt, runParser)
import Language.Evaluator (eval, evalStmt, GokuError(..))
import Language.Context (Context)
import Control.Exception (try, SomeException)

main :: IO ()
main = hspec $
  describe "Goku Language" $ do
    describe "Parser" $ do
      it "parses integer literals" $ do
        runParser parseExpr "5" `shouldBe` Right (LitInt 5)
      it "parses boolean literals" $ do
        runParser parseExpr "true" `shouldBe` Right (LitBool True)
      it "parses variables" $ do
        runParser parseExpr "x" `shouldBe` Right (Var "x")
      it "parses lambda expressions" $ do
        runParser parseExpr "\\x -> x" `shouldBe` Right (Lam "x" TInt (Var "x"))
      it "parses application expressions" $ do
        runParser parseExpr "f x" `shouldBe` Right (App (Var "f") (Var "x"))
      it "parses assert statements" $ do
        runParser parseStmt "assert true" `shouldBe` Right (Assert (LitBool True))

    describe "Evaluator" $ do
      it "evaluates integer literals" $ do
        (val, _) <- eval (LitInt 5) []
        val `shouldBe` LitInt 5
      it "evaluates boolean literals" $ do
        (val, _) <- eval (LitBool True) []
        val `shouldBe` LitBool True
      it "evaluates variables" $ do
        (val, _) <- eval (Var "x") [("x", LitInt 5)]
        val `shouldBe` LitInt 5
      it "evaluates lambda expressions" $ do
        (val, _) <- eval (Lam "x" TInt (Var "x")) []
        val `shouldBe` Lam "x" TInt (Var "x")
      it "evaluates application expressions" $ do
        (val, _) <- eval (App (Lam "x" TInt (Var "x")) (LitInt 5)) []
        val `shouldBe` LitInt 5
      it "evaluates assert statements" $ do
        evalStmt (Assert (LitBool True)) [] `shouldReturn` []
        result <- try (evalStmt (Assert (LitBool False)) []) :: IO (Either GokuError Context)
        case result of
          Left (GokuError msg) -> msg `shouldBe` "Assertion failed!"
          Right _ -> expectationFailure "Expected an assertion failure, but got success."