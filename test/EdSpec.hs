module EdSpec where

import           Coreutils.Ed
import           Test.Hspec

spec :: Spec
spec = do
    describe "printLine" $ do
        it "works" $ do
            let s = State ["a", "b", "c"] 1 ""
            printLine s `shouldBe` (s, "a")

        it "out of bounds" $ do
            let s = State ["a", "b", "c"] 0 ""
            printLine s `shouldBe` failure s "invalid address"

        it "resets error" $ do
            let s = State ["a", "b", "c"] 1 "error"
            printLine s `shouldBe` (s { err = "" }, "a")

    describe "changePosition" $ do
        it "works" $ do
            let s = State ["a", "b", "c"] 1 ""
            changePosition s "1" `shouldBe` (s, "")
            changePosition s "2" `shouldBe` (s { position = 2 }, "")
            changePosition s "3" `shouldBe` (s { position = 3 }, "")

        it "invalid address" $ do
            let s = State ["a", "b", "c"] 1 ""
            changePosition s "x" `shouldBe` failure s "invalid address"

        it "out of bounds" $ do
            let s = State ["a", "b", "c"] 1 ""
            changePosition s "0" `shouldBe` failure s "invalid address"
            changePosition s "4" `shouldBe` failure s "invalid address"

    describe "addresses" $ do
        let s = State ["a", "b", "c"] 2 ""

        it "position" $ do
            address s "." `shouldBe` Right (Position 2)
            address s "$" `shouldBe` Right (Position 3)
            address s "-" `shouldBe` Right (Position 1)
            address s "^" `shouldBe` Right (Position 1)
            address s "+" `shouldBe` Right (Position 3)

        it "complex position" $ do
            address s "-1" `shouldBe` Right (Position 1)
            address s "^1" `shouldBe` Right (Position 1)

        it "range" $ do
            address s "," `shouldBe` Right (Range 1 3)
            address s "%" `shouldBe` Right (Range 1 3)
            address s ";" `shouldBe` Right (Range 2 3)
            address s "x" `shouldBe` Left "invalid address"

    describe "toNumber" $ do
        it "works" $ do
            toNumber "1"   `shouldBe` Right 1
            toNumber "233" `shouldBe` Right 233

        it "invalid address" $ do
            toNumber "x" `shouldBe` Left "invalid address"
            toNumber ""  `shouldBe` Left "invalid address"

    describe "validateAddress" $ do
        let s = State ["a", "b", "c"] 2 ""
        let test = validateAddress s

        it "position" $ do
            test (Position 1) `shouldBe` Right (Position 1)
            test (Position 2) `shouldBe` Right (Position 2)
            test (Position 3) `shouldBe` Right (Position 3)

            test (Position 0) `shouldBe` Left "invalid address"
            test (Position 4) `shouldBe` Left "invalid address"

        it "range" $ do
            test (Range 1 3)  `shouldBe` Right (Range 1 3)
            test (Range 2 2)  `shouldBe` Right (Range 2 2)
            test (Range 2 3)  `shouldBe` Right (Range 2 3)
            test (Range 3 3)  `shouldBe` Right (Range 3 3)

            test (Range 0 3)  `shouldBe` Left "invalid address"
            test (Range 1 4)  `shouldBe` Left "invalid address"
            test (Range 3 2)  `shouldBe` Left "invalid address"
