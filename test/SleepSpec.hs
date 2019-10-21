module SleepSpec where

import Coreutils.Sleep

import Test.Hspec

spec :: Spec
spec = do
        -- happy path
        describe "parse" $
            it "seconds" $
                parse "1" `shouldBe` Just 1

        describe "parse" $
            it "seconds" $
                parse "1238" `shouldBe` Just 1238

        describe "parse" $
            it "seconds" $
                parse "1s" `shouldBe` Just 1

        describe "parse" $
            it "seconds" $
                parse "1238s" `shouldBe` Just 1238

        describe "parse" $
            it "minutes" $
                parse "1m" `shouldBe` Just 60

        describe "parse" $
            it "minutes" $
                parse "3m" `shouldBe` Just 180

        describe "parse" $
            it "days" $
                parse "1d" `shouldBe` Just (60 * 60 * 24)

        -- errors
        describe "parse" $
            it "junk" $
                parse "1z" `shouldBe` Nothing

        describe "parse" $
            it "junk" $
                parse "junk" `shouldBe` Nothing
