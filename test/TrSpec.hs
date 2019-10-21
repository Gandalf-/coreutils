module TrSpec where

import Coreutils.Tr

import Test.Hspec

spec :: Spec
spec = do
        -- happy path
        describe "delete" $
            it "simple" $
                runner [Delete, Set "abc"] "abcdef"
                `shouldBe` "def"

        describe "delete" $
            it "irrelevant" $
                runner [Delete, Set "xzy"] "abcdef"
                `shouldBe` "abcdef"

        describe "translate" $
            it "simple 1" $
                runner [Set "abc", Set "123"] "abcdef"
                `shouldBe` "123def"

        describe "translate" $
            it "simple 2" $
                runner [Set lower, Set upper] "abcdef"
                `shouldBe` "ABCDEF"
