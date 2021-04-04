module SleepSpec where

import Coreutils.Sleep

import Test.Hspec

spec :: Spec
spec =
        -- happy path
        describe "parse" $ do
            it "seconds" $
                parse "1" `shouldBe` Just 1

            it "seconds" $
                parse "1238" `shouldBe` Just 1238

            it "seconds" $
                parse "1s" `shouldBe` Just 1

            it "seconds" $
                parse "1238s" `shouldBe` Just 1238

            it "minutes" $
                parse "1m" `shouldBe` Just 60

            it "minutes" $
                parse "3m" `shouldBe` Just 180

            it "days" $
                parse "1d" `shouldBe` Just (60 * 60 * 24)

            it "weeks" $
                parse "2w" `shouldBe` Just (2 * 60 * 60 * 24 * 7)

            it "decimals" $
                parse "0.5m" `shouldBe` Just 30

            it "decimals no leading zero" $
                parse ".5" `shouldBe` Just 0.5

            -- errors
            it "junk" $
                parse "1z" `shouldBe` Nothing

            it "junk" $
                parse "junk" `shouldBe` Nothing
