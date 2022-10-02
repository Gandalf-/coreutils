{-# LANGUAGE OverloadedStrings #-}

module HeadSpec where

import           Coreutils.Head
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as C
import           Data.Either
import qualified Streaming.ByteString.Char8 as Q
import           Test.Hspec

spec :: Spec
spec = do
    describe "parse" $ do
        it "positiveInt" $ do
            parseNumber "123" `shouldBe` Right 123
            parseNumber "0"   `shouldBe` Right 0

            (parseNumber "a" :: Either String Int)  `shouldSatisfy` isLeft
            (parseNumber "-3" :: Either String Int) `shouldSatisfy` isLeft

        it "bytes invalid" $ do
            parseBytes "a"    `shouldSatisfy` isLeft
            parseBytes "-3"   `shouldSatisfy` isLeft
            parseBytes "1foo" `shouldSatisfy` isLeft
            parseBytes "foo"  `shouldSatisfy` isLeft
            parseBytes "MB"   `shouldSatisfy` isLeft

        it "bytes valid" $ do
            parseBytes "123" `shouldBe` Right 123
            parseBytes "0"   `shouldBe` Right 0
            parseBytes "1b"  `shouldBe` Right 512
            parseBytes "4kB" `shouldBe` Right 4000
            parseBytes "4K"  `shouldBe` Right 4096
            parseBytes "8MB" `shouldBe` Right 8000000
            parseBytes "1M"  `shouldBe` Right 1048576
            parseBytes "2PB" `shouldBe` Right 2000000000000000

    describe "runtime" $ do
        it "not quiet, not last" $ do
            let rt = getRuntime (Options (HeadBytes 1) False False) "-"
            prefix rt  `shouldBe` "==> - <==\n"
            postfix rt `shouldBe` "\n"

        it "not quiet, last" $ do
            let rt = getRuntime (Options (HeadBytes 1) False True) "-"
            prefix rt  `shouldBe` "==> - <==\n"
            postfix rt `shouldBe` C.empty

        it "quiet, not last" $ do
            let rt = getRuntime (Options (HeadBytes 1) True False) "-"
            prefix rt  `shouldBe` C.empty
            postfix rt `shouldBe` C.empty

        it "quiet, last" $ do
            let rt = getRuntime (Options (HeadBytes 1) True True) "-"
            prefix rt  `shouldBe` C.empty
            postfix rt `shouldBe` C.empty

    describe "bytes" $ do
        it "normal" $ do
            let os = quiet $ defaultOptions { optAction = HeadBytes 5 }
            test os "123456789" `shouldReturn` "12345"

        it "overflow" $ do
            let os = quiet $ defaultOptions { optAction = HeadBytes 999 }
            test os "123456789" `shouldReturn` "123456789"

        it "zero" $ do
            let os = quiet $ defaultOptions { optAction = HeadBytes 0 }
            test os "123456789" `shouldReturn` C.empty

    describe "lines" $ do
        it "normal" $ do
            let os = quiet $ defaultOptions { optAction = HeadLines 2 }
            test os (C.unlines ["1", "2", "3"]) `shouldReturn` C.unlines ["1", "2"]

        it "overflow" $ do
            let os = quiet $ defaultOptions { optAction = HeadLines 999 }
            test os (C.unlines ["1", "2", "3"]) `shouldReturn` C.unlines ["1", "2", "3"]

        it "zero" $ do
            let os = quiet $ defaultOptions { optAction = HeadLines 0 }
            test os (C.unlines ["1", "2", "3"]) `shouldReturn` C.empty

    describe "prefix" $ do
        it "middle" $ do
            let os = defaultOptions { optAction = HeadBytes 5 }
            test os "123456789" `shouldReturn` "==> - <==\n12345\n"

        it "last" $ do
            let os = defaultOptions { optAction = HeadBytes 5, optLast = True }
            test os "123456789" `shouldReturn` "==> - <==\n12345"

    where
        quiet os = os { optQuiet = True }

test :: Options -> ByteString -> IO ByteString
test os bs = Q.toStrict_ $ header rt $ Q.fromStrict bs
    where
        rt = getRuntime os "-"
