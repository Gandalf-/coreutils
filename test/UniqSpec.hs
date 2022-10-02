{-# LANGUAGE OverloadedStrings #-}
module UniqSpec where

import           Coreutils.Uniq
import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Char8      as C
import qualified Streaming.ByteString.Char8 as Q
import           Test.Hspec

spec :: Spec
spec = do
    describe "prepare" $ do
        it "ignore case" $ do
            let os = defaultOptions { optIgnoreCase = True }
            preparer os "heLLo" `shouldBe` "hello"

        it "skip chars" $ do
            let os = defaultOptions { optSkipChars = 2 }
            preparer os "hello" `shouldBe` "llo"

        it "skip fields" $ do
            let os = defaultOptions { optSkipFields = 1 }
            preparer os "apple sauce" `shouldBe` "sauce"

        it "combined" $ do
            let os = defaultOptions {
                optIgnoreCase = True, optSkipChars = 2, optSkipFields = 1
            }
            preparer os "boop HELLO" `shouldBe` "hello"

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

    describe "formatting" $ do
        it "defaults" $ do
            let rt = getRuntime defaultOptions
            format rt 9 "hello" `shouldBe` "hello"

        it "counts" $ do
            let rt = getRuntime defaultOptions { optCount = True }
            format rt     9 "hello" `shouldBe` "   9 hello"
            format rt    19 "hello" `shouldBe` "  19 hello"
            format rt  9919 "hello" `shouldBe` "9919 hello"
            format rt 19919 "hello" `shouldBe` "19919 hello"

    describe "uniq" $ do
        it "defaults" $ do
            test defaultOptions ["A", "a", "b", "", "cd", "ed"]

        it "ignore case" $ do
            let os = defaultOptions { optIgnoreCase = True }
            test os ["A", "b", "", "cd", "ed"]

        it "unique" $ do
            let os = defaultOptions { optUnique = True }
            test os ["A", "b", ""]

        it "repeated" $ do
            let os = defaultOptions { optRepeated = True }
            test os ["a", "cd", "ed"]

        it "all repeated" $ do
            let os = defaultOptions { optAllRepeated = True }
            test os ["a", "a", "cd", "cd", "ed", "ed"]

        it "skip chars" $ do
            let os = defaultOptions { optSkipChars = 1 }
            test os ["A", "cd"]

        it "count, unique" $ do
            let os = defaultOptions { optUnique = True, optCount = True }
            test os ["   1 A", "   1 b", "   1 "]

        it "all repeated, skip char" $ do
            let os = defaultOptions { optAllRepeated = True, optSkipChars = 1 }
            test os ["A", "a", "a", "b", "", "cd", "cd", "ed", "ed"]

        it "all repeated, ignore case" $ do
            let os = defaultOptions { optAllRepeated = True, optIgnoreCase = True }
            test os ["A", "a", "a", "cd", "cd", "ed", "ed"]

        it "repeated, ignore case, count" $ do
            let os = defaultOptions {
                optRepeated = True, optIgnoreCase = True, optCount = True
            }
            test os ["   3 A", "   2 cd", "   2 ed"]

        it "unique, ignore case" $ do
            let os = defaultOptions { optUnique = True, optIgnoreCase = True }
            test os ["b", ""]
    where
        drt = getRuntime defaultOptions
        urt = getRuntime defaultOptions { optUnique = True }
        rrt = getRuntime defaultOptions { optRepeated = True }
        art = getRuntime defaultOptions { optAllRepeated = True }

        dst = getState drt

        txt = C.unlines ["A", "a", "a", "b", "", "cd", "cd", "ed", "ed"]
        test rt ex = run (getState $ getRuntime rt) txt `shouldReturn` C.unlines ex


uniquely :: Runtime -> [Line] -> IO [Line]
uniquely rt is =
        C.lines <$> run st (C.unlines is)
    where
        st = getState rt

run :: UniqState -> ByteString -> IO ByteString
run st s = do
    (bs, ns) <- worker Q.toStrict_ (Q.fromStrict s) st
    pure $ bs <> finalize ns
