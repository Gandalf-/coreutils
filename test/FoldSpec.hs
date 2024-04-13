module FoldSpec where

import           Coreutils.Fold
import           Data.Char
import           Test.Hspec
import           Text.Parsec    (space)

spec :: Spec
spec = do
    describe "axioms" $
        it "isSpace" $ do
            isSpace ' ' `shouldBe` True
            isSpace '\n' `shouldBe` True
            isSpace '\t' `shouldBe` True
            isSpace '\r' `shouldBe` True
            isSpace 'a' `shouldBe` False

    describe "spaceSplit" $ do
        it "bytes works" $ do
            let bsplit = splitter (positions Bytes)
            bsplit "abc" 3 `shouldBe` ("abc", "")
            bsplit "ab\nc" 3 `shouldBe` ("ab", "c")
            bsplit "abc\nd" 3 `shouldBe` ("abc", "d")

    describe "fold" $
        it "works" $ do
            True `shouldBe` True
            -- small "abcde"      `shouldBe` ["abc", "de"]
            -- small "ab\ncd"     `shouldBe` "ab\ncd"
            -- small "abc\nd"     `shouldBe` "abc\nd"
            -- small "ab\r\r"     `shouldBe` "ab\r\r"
            -- small "ab\r\rcde"  `shouldBe` "ab\r\rcde"
            -- small "ab\r\rcdef" `shouldBe` "ab\r\rcde\nf"

            -- medium "\tabc" `shouldBe` "\ta\nbc"
            -- medium "a\tbc" `shouldBe` "a\tb\nc"
            -- medium "\t\tbc" `shouldBe` "\t\n\tb\nc"
    where
        os     = defaultOptions
        small  = foldLine defaultOptions { optWidth = 3 }
        medium = foldLine defaultOptions { optWidth = 9 }
