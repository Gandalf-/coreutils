module FoldSpec where

import           Coreutils.Fold
import           Test.Hspec

spec :: Spec
spec =
    describe "fold" $
        it "works" $ do
            small "abcde"   `shouldBe` "abc\nde"
            -- small "abc\bde" `shouldBe` "ab\nde"
    where
        small = folder defaultOptions { optWidth = 3 }
