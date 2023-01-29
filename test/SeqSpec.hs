{-# OPTIONS_GHC -Wno-type-defaults #-}

module SeqSpec where

import           Coreutils.Seq
import           Data.Either
import           Test.Hspec
import           Text.Printf   (printf)

spec :: Spec
spec = parallel $ do
    describe "printf" $
        it "formats" $ do
            let v1 = 3.00 :: Double
                v2 = 3.14 :: Double
            printf "%.0f" v1 `shouldBe` "3"
            printf "%g"   v1 `shouldBe` "3.0"
            printf "%g"   v2 `shouldBe` "3.14"

    describe "getBounds" $ do
        it "orders formats" $ do
            maximum [IntFormat, IntFormat]     `shouldBe` IntFormat
            maximum [IntFormat, DecFormat 1]   `shouldBe` DecFormat 1
            maximum [DecFormat 1, DecFormat 2] `shouldBe` DecFormat 2

        it "last" $ do
            getBounds ["5"]    `shouldBe` Right (Bounds 1 1 5, IntFormat)
            getBounds ["5.5"]  `shouldBe` Right (Bounds 1 1 5.5, IntFormat)
            getBounds [".5"]   `shouldBe` Right (Bounds 1 1 0.5, IntFormat)
            getBounds ["-5"]   `shouldBe` Right (Bounds 1 1 (-5), IntFormat)
            getBounds ["-.5"]  `shouldBe` Right (Bounds 1 1 (-0.5), IntFormat)

        it "start, last" $ do
            getBounds ["2", "5"]     `shouldBe` Right (Bounds 2 1 5, IntFormat)
            getBounds ["2.0", "5"]   `shouldBe` Right (Bounds 2 1 5, DecFormat 1)
            getBounds ["2.0", "5.0"] `shouldBe` Right (Bounds 2 1 5, DecFormat 1)

        it "start, inc, last" $ do
            getBounds ["1", "2", "5"]   `shouldBe` Right (Bounds 1 2 5, IntFormat)
            getBounds ["1", "2", "5.3"] `shouldBe` Right (Bounds 1 2 5.3, IntFormat)
            getBounds ["1", ".1", "5"]  `shouldBe` Right (Bounds 1 0.1 5, DecFormat 1)
            getBounds ["0.1", "1", "5"] `shouldBe` Right (Bounds 0.1 1 5, DecFormat 1)

            getBounds ["0.1", "0", "5"]    `shouldSatisfy` isLeft
            getBounds ["0.1", "a", "5"]    `shouldSatisfy` isLeft

            getBounds []                   `shouldBe` Left "too few arguments"
            getBounds ["0", "a", "5", "5"] `shouldBe` Left "too many arguments"

    describe "expand" $ do
        it "increasing" $ do
            expand (Bounds 1 1 5)   `shouldBe` [1, 2, 3, 4, 5]
            expand (Bounds 1 2 5)   `shouldBe` [1, 3, 5]
            expand (Bounds 1.5 1 5) `shouldBe` [1.5, 2.5, 3.5, 4.5]
            expand (Bounds 1 1.5 5) `shouldBe` [1, 2.5, 4]

        it "decreasing" $ do
            expand (Bounds 5 (-1) 1)   `shouldBe` [5, 4, 3, 2, 1]
            expand (Bounds 5.5 (-1) 1) `shouldBe` [5.5, 4.5, 3.5, 2.5, 1.5]

        it "nothing" $ do
            expand (Bounds 1 1 0) `shouldBe` []
            expand (Bounds 5 1 0) `shouldBe` []

    describe "getRuntime" $ do
        it "defaults" $ do
            let (Right rt) = getRuntime defaultOptions ["5"]
            format rt 3 `shouldBe` "3\n"
            values rt `shouldBe` [1, 2, 3, 4, 5]

        xit "decimal" $ do
            let (Right rt) = getRuntime defaultOptions ["1", "0.1", "1.5"]
            map (format rt) (values rt)
                `shouldBe` ["1\n", "1.1\n", "1.2\n", "1.3\n", "1.4\n", "1.5\n"]

        it "format" $ do
            let opts = defaultOptions { optFormat = Just "%.5f" }
            let (Right rt) = getRuntime opts ["5"]
            format rt 3 `shouldBe` "3.00000\n"

        it "separator" $ do
            let opts = defaultOptions { optSeparator = "hello" }
            let (Right rt) = getRuntime opts ["5"]
            format rt 3 `shouldBe` "3hello"
