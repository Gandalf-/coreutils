module FoldSpec where

import           Coreutils.Fold
import           Test.Hspec

spec :: Spec
spec = do
    describe "enumerate" $
        it "works" $ do
            enumerate os "abc"   `shouldBe` [(1, 'a'), (1, 'b'), (1, 'c')]
            enumerate os "a\rc"  `shouldBe` [(1, 'a'), (-1, '\r'), (1, 'c')]
            enumerate os "a\t\b" `shouldBe` [(1, 'a'), (8, '\t'), (-1, '\b')]

    describe "fold" $
        it "works" $ do
            small "abcde"      `shouldBe` "abc\nde"
            small "ab\ncd"     `shouldBe` "ab\ncd"
            small "abc\nd"     `shouldBe` "abc\nd"
            small "ab\r\r"     `shouldBe` "ab\r\r"
            small "ab\r\rcde"  `shouldBe` "ab\r\rcde"
            small "ab\r\rcdef" `shouldBe` "ab\r\rcde\nf"

            medium "\tabc" `shouldBe` "\ta\nbc"
            -- medium "a\tbc" `shouldBe` "a\tb\nc"
            -- medium "\t\tbc" `shouldBe` "\t\n\tb\nc"
    where
        os     = defaultOptions
        small  = folder defaultOptions { optWidth = 3 }
        medium = folder defaultOptions { optWidth = 9 }
