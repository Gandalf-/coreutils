module SleepSpec where

import           Coreutils.Sleep

import           Test.Hspec

spec :: Spec
spec = parallel $
    -- happy path
    describe "parse" $ do
        it "seconds" $ do
            parse "1" `shouldBe` Just 1
            parse "1238" `shouldBe` Just 1238
            parse "1s" `shouldBe` Just 1
            parse "1238s" `shouldBe` Just 1238

        it "minutes" $ do
            parse "1m" `shouldBe` Just 60
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
        it "junk" $ do
            parse "1z" `shouldBe` Nothing
            parse "junk" `shouldBe` Nothing
