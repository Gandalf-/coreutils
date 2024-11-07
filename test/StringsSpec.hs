{-# LANGUAGE OverloadedStrings #-}

module StringsSpec where

import           Data.Either
import           Test.Hspec

import           Coreutils.Strings

spec :: Spec
spec = do
    describe "filters" $ do
        it "by printable" $ do
            strings 1 "ab"     `shouldBe` ["ab"]
            strings 1 "ab\0"   `shouldBe` ["ab"]
            strings 1 "ab\0cd" `shouldBe` ["ab", "cd"]
            strings 1 "a\0\0d" `shouldBe` ["a", "d"]
            strings 1 "a\0\1d" `shouldBe` ["a", "d"]

            strings 2 "ab\n\ncd" `shouldBe` ["ab", "cd"]

            strings 1 "ab\0\0\0cd" `shouldBe` ["ab", "cd"]

        it "by length" $ do
            strings 1 "hello" `shouldBe` ["hello"]
            strings 4 "hello" `shouldBe` ["hello"]
            strings 5 "hello" `shouldBe` ["hello"]
            strings 6 "hello" `shouldBe` []

    describe "arg parsing" $ do
        it "valid" $ do
            parseArgs ["hello"]        `shouldBe` Right (Options 4, ["hello"])
            parseArgs ["a", "b"]       `shouldBe` Right (Options 4, ["a", "b"])
            parseArgs ["-n", "9", "a"] `shouldBe` Right (Options 9, ["a"])
            parseArgs ["-9", "hello"]  `shouldBe` Right (Options 9, ["hello"])
            parseArgs ["-99", "a"]     `shouldBe` Right (Options 99, ["a"])
            parseArgs []               `shouldBe` Right (Options 4, [])

        it "invalid" $ do
            parseArgs ["-n", "!"]  `shouldSatisfy` isLeft
            parseArgs ["-n"]       `shouldSatisfy` isLeft
            parseArgs ["-n", ".4"] `shouldSatisfy` isLeft
            parseArgs ["-b", "a"]  `shouldSatisfy` isLeft

        it "respects --" $ do
            parseArgs ["--", "4"]             `shouldBe` Right (Options 4, ["4"])
            parseArgs ["--", "--"]            `shouldBe` Right (Options 4, ["--"])
            parseArgs ["-n", "9", "--", "-n"] `shouldBe` Right (Options 9, ["-n"])
            parseArgs ["a", "--", "-n"]       `shouldBe` Right (Options 4, ["a", "-n"])

        it "invalid option formats" $ do
            parseArgs ["--n", "9", "file"] `shouldSatisfy` isLeft
            parseArgs ["-n9", "file"]      `shouldSatisfy` isLeft

        it "trailing options" $ do
            parseArgs ["file", "-n", "9"] `shouldBe` Right (Options 9, ["file"])

        it "mixed options and files" $ do
            parseArgs ["a", "-n", "9", "b"] `shouldBe` Right (Options 9, ["a", "b"])

        it "repeated options" $ do
            parseArgs ["-n", "9", "-10", "a"] `shouldBe` Right (Options 10, ["a"])
