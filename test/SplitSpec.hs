module SplitSpec where

import Data.List
import Coreutils.Split

import Test.Hspec

spec :: Spec
spec = do
        -- filenames generator
        describe "filenames" $
            it "alpha 2 plain" $
                take 3 (filenameGenerator "" False 2 "") `shouldBe` ["aa", "ab", "ac"]

        describe "filenames" $
            it "numeric 2 plain" $
                take 3 (filenameGenerator "" True 2 "") `shouldBe` ["00", "01", "02"]

        describe "filenames" $
            it "numeric 2 extra" $
                take 3 (filenameGenerator "" True 2 "zip") `shouldBe` ["00zip", "01zip", "02zip"]

        describe "filenames" $
            it "numeric 4 plain" $
                take 3 (filenameGenerator "" True 4 "") `shouldBe` ["0000", "0001", "0002"]

        describe "filenames" $
            it "ordering during rollover alpha" $ do
                let fns = take 20000 (filenameGenerator "" False 2 "")
                fns `shouldBe` sort fns

        describe "filenames" $
            it "ordering during rollover numeric" $ do
                let fns = take 20000 (filenameGenerator "" True 2 "")
                fns `shouldBe` sort fns

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
                adjustment "1k" `shouldBe` Just 1024

        describe "adjustment" $
            it "1kb" $
                adjustment "1kb" `shouldBe` Just 1000

        describe "adjustment" $
            it "1000kb == 1mb" $
                adjustment "1000kb" `shouldBe` adjustment "1mb"
