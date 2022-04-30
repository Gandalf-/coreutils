{-# LANGUAGE OverloadedStrings #-}

module AwkSpec where

import Data.Either
import           Data.Text (Text)
import           Text.Parsec

import Coreutils.Awk
import Test.Hspec

spec :: Spec
spec = do
    describe "fields" $
        it "works" $ do
            fields "a b c"          `shouldBe` ["a", "b", "c"]
            fields "apple b c"      `shouldBe` ["apple", "b", "c"]
            fields "  apple b c"    `shouldBe` ["apple", "b", "c"]
            fields "  a   b   c"    `shouldBe` ["a", "b", "c"]
            fields "  a   b   c   " `shouldBe` ["a", "b", "c"]

    describe "parse values" $ do
        it "strings" $ do
            pRun pValue "\"a\""      `shouldBe` Right (String "a")
            pRun pValue "\"apple \"" `shouldBe` Right (String "apple ")
            pRun pValue "\"123! \""  `shouldBe` Right (String "123! ")

        it "strings negative" $ do
            pRun pValue "\""      `shouldSatisfy` isLeft
            pRun pValue "\"apple" `shouldSatisfy` isLeft

        it "fields" $ do
            pRun pValue "$0"  `shouldBe` Right (FieldVar 0)
            pRun pValue "$1"  `shouldBe` Right (FieldVar 1)
            pRun pValue "$10" `shouldBe` Right (FieldVar 10)

        it "separator" $
            pRun pValue "," `shouldBe` Right Separator

    describe "parser" $ do
        it "empty" $ do
            pRun pEmpty ""      `shouldBe` Right EmptyExpr
            pRun pEmpty "   "   `shouldBe` Right EmptyExpr
            pRun pEmpty "{ }"   `shouldBe` Right EmptyExpr
            pRun pEmpty " { } " `shouldBe` Right EmptyExpr

        it "empty negative" $ do
            pRun pEmpty "a"    `shouldSatisfy` isLeft
            pRun pEmpty "   a" `shouldSatisfy` isLeft

        it "action" $ do
            pRun pAction "print $1"    `shouldBe` Right (PrintValue [FieldVar 1])
            pRun pAction "print $1"    `shouldBe` Right (PrintValue [FieldVar 1])
            pRun pAction "print \"a\"" `shouldBe` Right (PrintValue [String "a"])
            pRun pAction "print \"a\"" `shouldBe` Right (PrintValue [String "a"])

            pRun pAction "print $1 $2" `shouldBe`
                Right (PrintValue [FieldVar 1, FieldVar 2])

            pRun pAction "print \"a\" $20" `shouldBe`
                Right (PrintValue [String "a", FieldVar 20])

            pRun pAction "print $1, $2" `shouldBe`
                Right (PrintValue [FieldVar 1, Separator, FieldVar 2])

        it "action negative" $ do
            pRun pAction "junk" `shouldSatisfy` isLeft
            pRun pAction " "    `shouldSatisfy` isLeft

    describe "expression" $
        it "works" $ do
            pRun pExpr ""          `shouldBe` Right EmptyExpr
            pRun pExpr "{ print }" `shouldBe` Right (ActionExpr PrintAll)
            pRun pExpr "{ print $1 }" `shouldBe`
                Right (ActionExpr (PrintValue [FieldVar 1]))
            pRun pExpr " { print $1 $2} " `shouldBe`
                Right (ActionExpr (PrintValue [FieldVar 1, FieldVar 2]))


testParse :: Text -> Expr -> Expectation
testParse s e =
    case awkParse s of
        (Left p)  -> expectationFailure (show p)
        (Right o) -> o `shouldBe` e

pRun :: Parsec Text () a -> Text -> Either ParseError a
pRun p = parse (p <* eof) "test"
