module CmpSpec (spec) where

import Coreutils.Cmp
import Test.Hspec

spec :: Spec
spec = do
    describe "parseSkip" $ do
        it "error" $ do
            parseSkip "" `shouldBe` Nothing
            parseSkip "cmp" `shouldBe` Nothing
            parseSkip ":" `shouldBe` Nothing
            parseSkip "3:" `shouldBe` Nothing
            parseSkip ":3" `shouldBe` Nothing

        it "no negatives" $ do
            parseSkip "-3" `shouldBe` Nothing
            parseSkip "-3:-3" `shouldBe` Nothing
            parseSkip "3:-3" `shouldBe` Nothing

        it "single" $ do
            parseSkip "1" `shouldBe` Just (1, 0)
            parseSkip "22" `shouldBe` Just (22, 0)

        it "double" $ do
            parseSkip "1:1" `shouldBe` Just (1, 1)
            parseSkip "22:0" `shouldBe` Just (22, 0)
            parseSkip "2:20" `shouldBe` Just (2, 20)
            parseSkip "002:0020" `shouldBe` Just (2, 20)

        it "single suffix" $ do
            parseSkip "1K" `shouldBe` Just (1024, 0)
            parseSkip "1kB" `shouldBe` Just (1000, 0)

            parseSkip "1M" `shouldBe` Just (1024 ^ 2, 0)
            parseSkip "1mB" `shouldBe` Just (1000 ^ 2, 0)

        it "double suffix" $ do
            parseSkip "1K:34" `shouldBe` Just (1024, 34)
            parseSkip "0:1kB" `shouldBe` Just (0, 1000)

            parseSkip "1K:1M" `shouldBe` Just (1024, 1024 ^ 2)
            parseSkip "1mB" `shouldBe` Just (1000 ^ 2, 0)