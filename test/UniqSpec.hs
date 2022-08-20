{-# LANGUAGE OverloadedStrings #-}
module UniqSpec where

import Coreutils.Uniq
import Test.Hspec
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Char8 (ByteString)
import qualified Streaming.ByteString.Char8 as Q

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
            match drt True  1 `shouldBe` False
            -- a is unique
            match drt False 1 `shouldBe` True
            -- a wasn't unique but we're at the end
            match drt False 2 `shouldBe` True

        it "unique" $ do
            let rt = getRuntime defaultOptions { optUnique = True }
            -- not unique
            match rt True 1  `shouldBe` False
            -- only one seen before
            match rt False 1 `shouldBe` True
            -- seen several before
            match rt False 2 `shouldBe` False

        it "repeated" $ do
            let rt = getRuntime defaultOptions { optRepeated = True }
            -- not repeated
            match rt False 1 `shouldBe` False
            -- maybe not done repeating
            match rt True 1 `shouldBe` False
            -- done repeating
            match rt False 2 `shouldBe` True

        it "all repeated" $ do
            let rt = getRuntime defaultOptions { optAllRepeated = True }
            -- not repeated
            match rt False 1 `shouldBe` False
            -- repeating
            match rt True 1 `shouldBe` True
            -- still repeating
            match rt True 2 `shouldBe` True
            -- done repeating
            match rt False 2 `shouldBe` True

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
            uniquely drt ["a", "b", "c"] `shouldReturn` ["a", "b", "c"]
            uniquely drt ["a", "a", "c"] `shouldReturn` ["a", "c"]
            uniquely drt ["a", "a", "a"] `shouldReturn` ["a"]

        it "unique" $ do
            uniquely urt ["a", "b", "c"] `shouldReturn` ["a", "b", "c"]
            uniquely urt ["a", "a", "c"] `shouldReturn` ["c"]
            uniquely urt ["a", "a", "a"] `shouldReturn` []
            uniquely urt ["b", "a", "a"] `shouldReturn` ["b"]

        it "repeated" $ do
            uniquely rrt ["a", "b", "c"] `shouldReturn` []
            uniquely rrt ["a", "a", "c"] `shouldReturn` ["a"]
            uniquely rrt ["a", "a", "a"] `shouldReturn` ["a"]
            uniquely rrt ["b", "a", "a"] `shouldReturn` ["a"]

        it "all repeated" $ do
            uniquely art ["a", "b", "c"] `shouldReturn` []
            uniquely art ["a", "a", "c"] `shouldReturn` ["a", "a"]
            uniquely art ["a", "a", "a"] `shouldReturn` ["a", "a", "a"]
            uniquely art ["b", "a", "a"] `shouldReturn` ["a", "a"]

    -- TODO formatting

    describe "io" $
        it "works" $
            run dst "a\nb\nc\n" `shouldReturn` "a\nb\nc\n"
    where
        drt = getRuntime defaultOptions
        urt = getRuntime defaultOptions { optUnique = True }
        rrt = getRuntime defaultOptions { optRepeated = True }
        art = getRuntime defaultOptions { optAllRepeated = True }

        dst = getState drt

uniquely :: Runtime -> [Line] -> IO [Line]
uniquely rt is =
        C.lines <$> run st (C.unlines is)
    where
        st = getState rt

run :: UniqState -> ByteString -> IO ByteString
run st s = do
    (bs, ns) <- worker Q.toStrict_ (Q.fromStrict s) st
    pure $ bs <> finalize ns
