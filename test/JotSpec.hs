module JotSpec where

import           Coreutils.Jot
import           Data.Either
import           Test.Hspec

spec :: Spec
spec = do
    describe "defaultStep" $
        it "works" $ do
            defaultStep 10 1 5 `shouldBe` (4 / 9)
            defaultStep 10 5 1 `shouldBe` negate (4 / 9)

            defaultStep 100 1 5 `shouldBe` (4 / 99)

    describe "parse" $ do
        it "integer" $ do
            parseInteger "234" `shouldBe` Right 234
            parseInteger "0"   `shouldBe` Right 0

            parseInteger "abc"  `shouldSatisfy` isLeft
            parseInteger "3abc" `shouldSatisfy` isLeft
            parseInteger ""     `shouldSatisfy` isLeft

        it "double" $ do
            parseDouble "1.4" `shouldBe` Right 1.4
            parseDouble "0"   `shouldBe` Right 0
            parseDouble "7"   `shouldBe` Right 7
            parseDouble "-7"   `shouldBe` Right (-7)
            parseDouble ".5"   `shouldBe` Right 0.5
            parseDouble "-.5"  `shouldBe` Right (-0.5)

            parseDouble "abc"  `shouldSatisfy` isLeft
            parseDouble "3abc" `shouldSatisfy` isLeft
            parseDouble ""     `shouldSatisfy` isLeft

        it "bounds" $ do
            parseBound "1.4" `shouldBe` Right 1.4
            parseBound "0"   `shouldBe` Right 0
            parseBound "a"   `shouldBe` Right 97
            parseBound "z"   `shouldBe` Right 122

            parseBound "abc"  `shouldSatisfy` isLeft
            parseBound "3abc" `shouldSatisfy` isLeft
            parseBound ""     `shouldSatisfy` isLeft

    describe "parseRange" $ do
        it "errors" $ do
            parseRange [] `shouldSatisfy` isLeft
            parseRange ["1", "2", "3", "4", "5"] `shouldSatisfy` isLeft

        it "defaults" $ do
            parseRange ["-"]                `shouldBe` Right defaultRange
            parseRange ["-", "-"]           `shouldBe` Right defaultRange
            parseRange ["-", "-", "-"]      `shouldBe` Right defaultRange
            parseRange ["-", "-", "-", "-"] `shouldBe` Right defaultRange

            parseRange [""]                `shouldBe` Right defaultRange
            parseRange ["", ""]            `shouldBe` Right defaultRange
            parseRange ["", "", ""]        `shouldBe` Right defaultRange
            parseRange ["", "", "", ""]    `shouldBe` Right defaultRange
{-
        it "reps" $ do
            parseRange ["5"]   `shouldBe` Right defaultRange { reps = 5 }
            parseRange ["5.4"] `shouldSatisfy` isLeft
            parseRange ["a"]   `shouldSatisfy` isLeft

        it "begins" $ do
            parseRange ["-", "5"]   `shouldBe` Right defaultRange { rLow = 5 }
            parseRange ["-", "5.1"] `shouldBe` Right defaultRange { rLow = 5.1 }
            parseRange ["-", "a"]   `shouldBe` Right defaultRange { rLow = 97 }
            parseRange ["-", "ab"]  `shouldSatisfy` isLeft

        it "ends" $ do
            parseRange ["-", "-", "5"]   `shouldBe` Right defaultRange { rHigh = 5 }
            parseRange ["-", "-", "5.1"] `shouldBe` Right defaultRange { rHigh = 5.1 }
            parseRange ["-", "-", "a"]   `shouldBe` Right defaultRange { rHigh = 97 }
            parseRange ["-", "-", "ab"]  `shouldSatisfy` isLeft

        it "step" $ do
            parseRange ["-", "-", "-", "5"]   `shouldBe` Right defaultRange { rStep = 5 }
            parseRange ["-", "-", "-", "5.1"] `shouldBe` Right defaultRange { rStep = 5.1 }
            parseRange ["-", "-", "-", "a"]   `shouldSatisfy` isLeft

            -- inverted low high means negative default step
            parseRange ["-", "5", "1", "-"] `shouldBe`
                Right defaultRange { rLow = 5, rHigh = 1, rStep = -1 }

            -- step is inferred based on reps and low -> high range
            parseRange ["5", "1", "2"] `shouldBe`
                Right Range { reps = 5, rLow = 1, rHigh = 2, rStep = 0.25 }
-}
