{-# LANGUAGE OverloadedStrings #-}
module UniqSpec where

import Coreutils.Uniq
import Test.Hspec
import qualified Data.ByteString.Char8 as C

spec :: Spec
spec = do
    describe "getRuntime" $ do
        it "defaults" $ do
            format drt 0 "123 Hello" `shouldBe` "123 Hello"
            prepare drt "123 Hello" `shouldBe` "123 Hello"
            -- match rt "abc" 0 "def" `shouldBe` True

        it "formats" $ do
            let rt = getRuntime defaultOptions { optCount = True }
            format rt 5 "hello" `shouldBe` "5 hello"

        it "prepares" $ do
            let rt = getRuntime defaultOptions { optIgnoreCase = True }
            prepare rt "heLLo" `shouldBe` "hello"

    describe "matches" $ do
        it "default dedupe" $ do
            -- not unique
            match drt "a" 1 "a" `shouldBe` Nothing
            -- a is unique
            match drt "a" 1 "b" `shouldBe` Just "a"
            -- a wasn't unique but we're at the end
            match drt "a" 2 "b" `shouldBe` Just "a"

        it "unique" $ do
            let rt = getRuntime defaultOptions { optUnique = True }
            -- not unique
            match rt "a" 1 "a" `shouldBe` Nothing
            -- only one seen before
            match rt "a" 1 "b" `shouldBe` Just "a"
            -- seen several before
            match rt "a" 2 "b" `shouldBe` Nothing

        it "repeated" $ do
            let rt = getRuntime defaultOptions { optRepeated = True }
            -- not repeated
            match rt "a" 1 "b" `shouldBe` Nothing
            -- maybe not done repeating
            match rt "b" 1 "b" `shouldBe` Nothing
            -- done repeating
            match rt "b" 2 "c" `shouldBe` Just "b"

        it "all repeated" $ do
            let rt = getRuntime defaultOptions { optAllRepeated = True }
            -- not repeated
            match rt "a" 1 "b" `shouldBe` Nothing
            -- repeating
            match rt "b" 1 "b" `shouldBe` Just "b"
            -- still repeating
            match rt "b" 2 "b" `shouldBe` Just "b"
            -- done repeating
            match rt "b" 2 "c" `shouldBe` Just "b"

    describe "execute" $ do
        it "defaults" $ do
            let (st1, l1) = execute dst "a"
            l1 `shouldBe` C.empty
            previous st1 `shouldBe` "a"
            count st1 `shouldBe` 1

            let (st2, l2) = execute st1 "b"
            l2 `shouldBe` "a"
            previous st2 `shouldBe` "b"
            count st2 `shouldBe` 1

    describe "uniquely" $ do
        it "defaults" $ do
            uniquely drt ["a", "b", "c"] `shouldBe` ["a", "b", "c"]
            uniquely drt ["a", "a", "c"] `shouldBe` ["a", "c"]
            uniquely drt ["a", "a", "a"] `shouldBe` ["a"]

        it "unique" $ do
            uniquely urt ["a", "b", "c"] `shouldBe` ["a", "b", "c"]
            uniquely urt ["a", "a", "c"] `shouldBe` ["c"]
            uniquely urt ["a", "a", "a"] `shouldBe` []
            uniquely urt ["b", "a", "a"] `shouldBe` ["b"]

        it "repeated" $ do
            uniquely rrt ["a", "b", "c"] `shouldBe` []
            uniquely rrt ["a", "a", "c"] `shouldBe` ["a"]
            uniquely rrt ["a", "a", "a"] `shouldBe` ["a"]
            uniquely rrt ["b", "a", "a"] `shouldBe` ["a"]

        it "all repeated" $ do
            uniquely art ["a", "b", "c"] `shouldBe` []
            uniquely art ["a", "a", "c"] `shouldBe` ["a", "a"]
            uniquely art ["a", "a", "a"] `shouldBe` ["a", "a", "a"]
            uniquely art ["b", "a", "a"] `shouldBe` ["a", "a"]
    where
        drt = getRuntime defaultOptions
        urt = getRuntime defaultOptions { optUnique = True }
        rrt = getRuntime defaultOptions { optRepeated = True }
        art = getRuntime defaultOptions { optAllRepeated = True }

        dst = getState drt

uniquely :: Runtime -> [Line] -> [Line]
uniquely rt = filter (not . C.null) . go st
    where
        st = getState rt
        go st []
            | final (runtime st) (count st) = [previous st]
            | otherwise          = []
        go st (x:xs) = o : go newSt xs
            where
                (newSt, o) = execute st x
