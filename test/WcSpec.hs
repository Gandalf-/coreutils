{-# LANGUAGE OverloadedStrings #-}

module WcSpec where

import qualified Data.ByteString.Char8      as C
import qualified Streaming.ByteString.Char8 as Q
import           Test.Hspec

import           Coreutils.Wc

spec :: Spec
spec = parallel $ do
    describe "countWords" $ do
        it "empty input starting in space" $
            countWords True "" `shouldBe` (0, True)

        it "empty input starting in word" $
            countWords False "" `shouldBe` (0, False)

        it "single word" $
            countWords True "hello" `shouldBe` (1, False)

        it "two words" $
            countWords True "hello world" `shouldBe` (2, False)

        it "leading whitespace" $
            countWords True "  hello" `shouldBe` (1, False)

        it "trailing whitespace" $
            countWords True "hello  " `shouldBe` (1, True)

        it "only whitespace" $
            countWords True "   " `shouldBe` (0, True)

        it "only whitespace starting in word" $
            countWords False "   " `shouldBe` (0, True)

        it "word continuation across chunks" $
            -- previous chunk ended mid-word, this chunk continues it
            countWords False "ello world" `shouldBe` (1, False)

        it "word start after chunk boundary" $
            -- previous chunk ended on space, this chunk starts a word
            countWords True "hello" `shouldBe` (1, False)

        it "tabs and newlines count as whitespace" $
            countWords True "a\tb\nc" `shouldBe` (3, False)

        it "single character" $
            countWords True "x" `shouldBe` (1, False)

        it "single space" $
            countWords True " " `shouldBe` (0, True)

        it "multiple whitespace types between words" $
            countWords True "a \t\n b" `shouldBe` (2, False)

    describe "streamCounter" $ do
        it "empty input" $ do
            r <- run ""
            r `shouldBe` (0, 0, 0)

        it "single newline" $ do
            r <- run "\n"
            r `shouldBe` (1, 0, 1)

        it "single word with newline" $ do
            r <- run "hello\n"
            r `shouldBe` (1, 1, 6)

        it "multiple lines" $ do
            r <- run "hello world\ngoodbye\n"
            r `shouldBe` (2, 3, 20)

        it "no trailing newline" $ do
            r <- run "hello"
            r `shouldBe` (0, 1, 5)

        it "only whitespace" $ do
            r <- run "   \n"
            r `shouldBe` (1, 0, 4)

        it "multiple blank lines" $ do
            r <- run "\n\n\n"
            r `shouldBe` (3, 0, 3)

        it "tabs and mixed whitespace" $ do
            r <- run "a\tb\n c\n"
            r `shouldBe` (2, 3, 7)

run :: C.ByteString -> IO Counts
run = streamCounter . Q.fromStrict
