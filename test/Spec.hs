{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Language.Syntax
import Language.Parser (parseExpr, parseStmt, parseProgram, runParser)
import Language.Evaluator (eval, evalStmt, evalProgram, GokuError(..))
import Language.Context (Context, emptyContext)
import Control.Exception (try, SomeException)

main :: IO ()
main = hspec $ do
  describe "Goku Language Parser" $ do
    describe "Basic Expressions" $ do
      it "parses integer literals" $ do
        runParser parseExpr "5" `shouldBe` Right (LitInt 5)
      it "parses boolean literals" $ do
        runParser parseExpr "true" `shouldBe` Right (LitBool True)
        runParser parseExpr "false" `shouldBe` Right (LitBool False)
      it "parses variables" $ do
        runParser parseExpr "x" `shouldBe` Right (Var "x")

    describe "Arithmetic Expressions" $ do
      it "parses addition" $ do
        runParser parseExpr "2 + 3" `shouldBe` Right (Add (LitInt 2) (LitInt 3))
      it "parses subtraction" $ do
        runParser parseExpr "5 - 2" `shouldBe` Right (Sub (LitInt 5) (LitInt 2))
      it "parses multiplication" $ do
        runParser parseExpr "3 * 4" `shouldBe` Right (Mult (LitInt 3) (LitInt 4))
      it "parses division" $ do
        runParser parseExpr "8 / 2" `shouldBe` Right (Div (LitInt 8) (LitInt 2))
      it "parses integer division" $ do
        runParser parseExpr "7 // 2" `shouldBe` Right (IntDiv (LitInt 7) (LitInt 2))

    describe "Comparison Expressions" $ do
      it "parses equality" $ do
        runParser parseExpr "x == 5" `shouldBe` Right (Equals (Var "x") (LitInt 5))
      it "parses less than" $ do
        runParser parseExpr "x < 10" `shouldBe` Right (LessThan (Var "x") (LitInt 10))

    describe "String Expressions" $ do
      it "parses string literals" $ do
        runParser parseExpr "\"hello\"" `shouldBe` Right (LitString "hello")
      it "parses empty string" $ do
        runParser parseExpr "\"\"" `shouldBe` Right (LitString "")
      it "parses string with spaces" $ do
        runParser parseExpr "\"hello world\"" `shouldBe` Right (LitString "hello world")
      it "parses string concatenation" $ do
        runParser parseExpr "\"hello\" ++ \"world\"" `shouldBe` Right (Concat (LitString "hello") (LitString "world"))
      it "parses string concatenation with variables" $ do
        runParser parseExpr "x ++ \" world\"" `shouldBe` Right (Concat (Var "x") (LitString " world"))
      it "parses string equality" $ do
        runParser parseExpr "\"test\" == \"test\"" `shouldBe` Right (Equals (LitString "test") (LitString "test"))

    describe "Lambda Expressions" $ do
      it "parses single parameter lambda" $ do
        runParser parseExpr "(x) -> x" `shouldBe` Right (Lam ["x"] TInt (Var "x"))
      it "parses multiple parameter lambda" $ do
        runParser parseExpr "(x,y) -> x + y" `shouldBe` Right (Lam ["x","y"] TInt (Add (Var "x") (Var "y")))

    describe "Statements" $ do
      it "parses let statements" $ do
        runParser parseStmt "let x = 5" `shouldBe` Right (Let "x" TInt (LitInt 5))
      it "parses set statements" $ do
        runParser parseStmt "x = 10" `shouldBe` Right (Set "x" (LitInt 10))
      it "parses assert statements" $ do
        runParser parseStmt "assert (true)" `shouldBe` Right (Assert (LitBool True))
      it "parses print statements" $ do
        runParser parseStmt "print 42" `shouldBe` Right (Print (LitInt 42))
        runParser parseStmt "print (2 + 3)" `shouldBe` Right (Print (Add (LitInt 2) (LitInt 3)))

  describe "Goku Language Evaluator" $ do
    describe "Basic Expression Evaluation" $ do
      it "evaluates integer literals" $ do
        (val, _) <- eval (LitInt 5) []
        val `shouldBe` LitInt 5
      it "evaluates boolean literals" $ do
        (val, _) <- eval (LitBool True) []
        val `shouldBe` LitBool True
      it "evaluates variables" $ do
        (val, _) <- eval (Var "x") [("x", LitInt 5)]
        val `shouldBe` LitInt 5

    describe "Arithmetic Evaluation" $ do
      it "evaluates addition" $ do
        (val, _) <- eval (Add (LitInt 2) (LitInt 3)) []
        val `shouldBe` LitInt 5
      it "evaluates subtraction" $ do
        (val, _) <- eval (Sub (LitInt 10) (LitInt 4)) []
        val `shouldBe` LitInt 6
      it "evaluates multiplication" $ do
        (val, _) <- eval (Mult (LitInt 3) (LitInt 4)) []
        val `shouldBe` LitInt 12
      it "evaluates division" $ do
        (val, _) <- eval (Div (LitInt 8) (LitInt 2)) []
        val `shouldBe` LitInt 4
      it "evaluates integer division" $ do
        (val, _) <- eval (IntDiv (LitInt 7) (LitInt 2)) []
        val `shouldBe` LitInt 3

    describe "Lambda and Function Evaluation" $ do
      it "evaluates lambda expressions" $ do
        (val, _) <- eval (Lam ["x"] TInt (Var "x")) []
        val `shouldBe` Lam ["x"] TInt (Var "x")
      it "evaluates function application" $ do
        (val, _) <- eval (App (Lam ["x"] TInt (Var "x")) (LitInt 5)) []
        val `shouldBe` LitInt 5

    describe "String Evaluation" $ do
      it "evaluates string literals" $ do
        (val, _) <- eval (LitString "hello") []
        val `shouldBe` LitString "hello"
      it "evaluates string concatenation" $ do
        (val, _) <- eval (Concat (LitString "hello") (LitString " world")) []
        val `shouldBe` LitString "hello world"
      it "evaluates string equality" $ do
        (val, _) <- eval (Equals (LitString "test") (LitString "test")) []
        val `shouldBe` LitBool True
      it "evaluates string inequality" $ do
        (val, _) <- eval (Equals (LitString "test") (LitString "other")) []
        val `shouldBe` LitBool False

    describe "Statement Evaluation" $ do
      it "evaluates let statements" $ do
        newCtx <- evalStmt (Let "x" TInt (LitInt 5)) []
        newCtx `shouldBe` [("x", LitInt 5)]
      it "evaluates set statements (new variables)" $ do
        newCtx <- evalStmt (Set "x" (LitInt 10)) []
        newCtx `shouldBe` [("x", LitInt 10)]
      it "evaluates successful assert statements" $ do
        evalStmt (Assert (LitBool True)) [] `shouldReturn` []
      it "evaluates print statements" $ do
        evalStmt (Print (LitInt 42)) [] `shouldReturn` []
        evalStmt (Print (LitBool True)) [] `shouldReturn` []
      it "evaluates failed assert statements" $ do
        result <- try (evalStmt (Assert (LitBool False)) []) :: IO (Either GokuError Context)
        case result of
          Left (GokuError msg) -> msg `shouldBe` "Assertion failed!"
          Right _ -> expectationFailure "Expected an assertion failure, but got success."
