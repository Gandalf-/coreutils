{-# LANGUAGE OverloadedStrings #-}

module TacSpec where

import qualified Data.ByteString.Char8      as C
import qualified Streaming.ByteString.Char8 as Q
import           System.IO
import           System.IO.Temp

import           Coreutils.Tac
import           Test.Hspec

runFileTac :: C.ByteString -> IO C.ByteString
runFileTac input = withSystemTempFile "tac" $ \path h -> do
    C.hPut h input
    hFlush h
    hSeek h AbsoluteSeek 0
    Q.toStrict_ (fileTac h)

spec :: Spec
spec = parallel $ do
    describe "locations" $ do
        it "zero zero" $
            locations 0 1000 `shouldBe` [(0, 0)]

        it "single smaller block" $
            locations 500 1000 `shouldBe` [(0, 500)]

        it "multiple blocks align" $
            locations 1000 500 `shouldBe` [(500, 500), (0, 500)]

        it "multiple blocks misalign" $
            locations 1020 500 `shouldBe` [(520, 500), (20, 500), (0, 20)]

    describe "reverseLines" $ do
        it "empty input" $
            reverseLines "" `shouldBe` ""

        it "trailing newline preserved" $
            reverseLines "a\nb\nc\n" `shouldBe` "c\nb\na\n"

        it "no trailing newline preserved" $
            reverseLines "hello" `shouldBe` "hello"

        it "no trailing newline multiline" $
            reverseLines "a\nb\nc" `shouldBe` "c\nb\na"

        it "leading empty line moves to end" $
            reverseLines "\na\nb\nc\n" `shouldBe` "c\nb\na\n\n"

        it "single newline" $
            reverseLines "\n" `shouldBe` "\n"

    describe "endsWithLF" $ do
        it "empty file" $ withSystemTempFile "tac" $ \_ h -> do
            endsWithLF h 0 `shouldReturn` False

        it "ends with newline" $ withSystemTempFile "tac" $ \_ h -> do
            C.hPut h "hello\n"
            hFlush h
            endsWithLF h 6 `shouldReturn` True

        it "no trailing newline" $ withSystemTempFile "tac" $ \_ h -> do
            C.hPut h "hello"
            hFlush h
            endsWithLF h 5 `shouldReturn` False

        it "single newline" $ withSystemTempFile "tac" $ \_ h -> do
            C.hPut h "\n"
            hFlush h
            endsWithLF h 1 `shouldReturn` True

    describe "fileTac" $ do
        it "basic" $
            runFileTac "a\nb\nc\n" `shouldReturn` "c\nb\na\n"

        it "preceding newline" $
            runFileTac "\na\nb\nc\n" `shouldReturn` "c\nb\na\n\n"
