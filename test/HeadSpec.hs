{-# LANGUAGE OverloadedStrings #-}

module HeadSpec where

import           Data.ByteString.Lazy  (ByteString)

import Coreutils.Head
import Test.Hspec

spec :: Spec
spec = do
        -- characters
        describe "chars op" $
            it "zero" $
                charsOp 0 word `shouldBe` ""

        describe "chars op" $
            it "1" $
                charsOp 1 word `shouldBe` "h"

        describe "chars op" $
            it "2" $
                charsOp 2 word `shouldBe` "he"

        describe "chars op" $
            it "overflow" $
                charsOp 200 word `shouldBe` "hello"

        describe "chars op" $
            it "-1" $
                charsOp (-1) word `shouldBe` "o"

        describe "chars op" $
            it "-2" $
                charsOp (-2) word `shouldBe` "lo"

        describe "chars op" $
            it "negative overflow" $
                charsOp (-200) word `shouldBe` "hello"

        -- lines
        describe "lines op" $
            it "zero" $
                linesOp 0 text `shouldBe` ""

        describe "lines op" $
            it "1" $
                linesOp 1 text `shouldBe` "hello\n"

        describe "lines op" $
            it "2" $
                linesOp 2 text `shouldBe` "hello\nthere\n"

        describe "lines op" $
            it "3" $
                linesOp 3 text `shouldBe` "hello\nthere\napple\n"

        describe "lines op" $
            it "overflow" $
                linesOp 200 text `shouldBe` text

        describe "lines op" $
            it "-1" $
                linesOp (-1) text `shouldBe` "sauce\n"

        describe "lines op" $
            it "-2" $
                linesOp (-2) text `shouldBe` "apple\nsauce\n"

        describe "lines op" $
            it "-3" $
                linesOp (-3) text `shouldBe` "there\napple\nsauce\n"

        describe "lines op" $
            it "negative overflow" $
                linesOp (-200) text `shouldBe` text


word :: ByteString
word = "hello"

text :: ByteString
text = "hello\nthere\napple\nsauce\n"
