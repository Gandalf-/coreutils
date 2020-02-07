module WcSpec where

import Coreutils.Wc
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
        -- | chars
        describe "chars" $
            it "zero" $
                charCount (wc' "") `shouldBe` 0

        describe "chars" $
            it "1" $
                charCount (wc' "a") `shouldBe` 1

        describe "chars" $
            it "property" $
                property $ \x ->
                    charCount (wc' x) `shouldBe` length x

        -- | words
        describe "words" $
            it "simple" $
                wordCount (wc' "a b c d") `shouldBe` 4

        describe "words" $
            it "extra spaces" $
                wordCount (wc' "a    b c d") `shouldBe` 4

        describe "words" $
            it "newlines" $
                wordCount (wc' "a  \n b c d") `shouldBe` 4

        describe "chars" $
            it "property" $
                property $ \x ->
                    wordCount (wc' x) `shouldBe` length (words x)


large :: String
large = replicate 999 'a'
