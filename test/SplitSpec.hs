module SplitSpec where

import Coreutils.Split

import Test.Hspec

spec :: Spec
spec = do
        -- suffix generator
        describe "suffix" $
            it "alpha 2 plain" $
                take 3 (suffixGenerator False 2 "") `shouldBe` ["aa", "ab", "ac"]

        describe "suffix" $
            it "numeric 2 plain" $
                take 3 (suffixGenerator True 2 "") `shouldBe` ["00", "01", "02"]

        describe "suffix" $
            it "numeric 2 extra" $
                take 3 (suffixGenerator True 2 "zip") `shouldBe` ["00zip", "01zip", "02zip"]

        describe "suffix" $
            it "numeric 4 plain" $
                take 3 (suffixGenerator True 4 "") `shouldBe` ["0000", "0001", "0002"]

        -- parsing
        describe "adjustment" $
            it "empty" $
                adjustment "" `shouldBe` Nothing

        describe "adjustment" $
            it "junk" $
                adjustment "apple" `shouldBe` Nothing

        describe "adjustment" $
            it "number junk" $
                adjustment "12839apple" `shouldBe` Nothing

        describe "adjustment" $
            it "suffix junk" $
                adjustment "12389ib" `shouldBe` Nothing

        describe "adjustment" $
            it "1k" $
                adjustment "1k" `shouldBe` Just 1000

        describe "adjustment" $
            it "1kb" $
                adjustment "1kb" `shouldBe` Just 1024

        describe "adjustment" $
            it "1024kb == 1mb" $
                adjustment "1024kb" `shouldBe` adjustment "1mb"
