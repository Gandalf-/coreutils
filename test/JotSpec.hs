module JotSpec where

import           Coreutils.Jot
import           Coreutils.Random (parse)
import           Data.Either
import           GHC.Conc.Sync    (par)
import           Test.Hspec

spec :: Spec
spec = do
    generalParsing
    missingOne
    missingTwo
    missingThree
    missingOther

missingOther :: Spec
missingOther = do
    -- count, lower bound, upper bound, step size
    describe "parse" $
        it "errors" $
            jotParse (Nothing, Nothing, Nothing, Nothing) `shouldSatisfy` isLeft

    describe "everything" $ do
        it "normal" $
            jotParse (Just 5, Just 1, Just 100, Just 1) `shouldBe`
                Right Range { start = 1, step = 1, count = 5 }

        it "negative" $
            jotParse (Just 10, Just 100, Just 1, Just (-1)) `shouldBe`
                Right Range { start = 100, step = -1, count = 10 }

        it "calculate positive" $
            jotParse (Just 100, Just 1, Just 10, Just 1) `shouldBe`
                Right Range { start = 1, step = 1, count = 10 }

        it "calculate negative" $
            jotParse (Just 100, Just 1, Just 10, Just 1) `shouldBe`
                Right Range { start = 1, step = 1, count = 10 }

        it "large step" $
            jotParse (Just 100, Just 1, Just 10, Just 5) `shouldBe`
                Right Range { start = 1, step = 5, count = 2 }

        it "impossible step" $
            jotParse (Just 100, Just 10, Just 1, Just 1) `shouldSatisfy` isLeft

missingOne :: Spec
missingOne = do
    describe "missing count" $ do
        it "normal" $
            jotParse (Nothing, Just 1, Just 100, Just 1) `shouldBe`
                Right Range { start = 1, step = 1, count = 100 }

        it "negative" $
            jotParse (Nothing, Just 100, Just 1, Just (-1)) `shouldBe`
                Right Range { start = 100, step = -1, count = 100 }

        it "calculate positive" $
            jotParse (Nothing, Just 1, Just 10, Just 1) `shouldBe`
                Right Range { start = 1, step = 1, count = 10 }

        it "calculate negative" $
            jotParse (Nothing, Just 1, Just 10, Just 1) `shouldBe`
                Right Range { start = 1, step = 1, count = 10 }

    describe "missing lower" $ do
        it "normal" $
            jotParse (Just 5, Nothing, Just 100, Just 1) `shouldBe`
                Right Range { start = 96, step = 1, count = 5 }

        it "negative" $
            jotParse (Just 5, Nothing, Just 1, Just (-1)) `shouldBe`
                Right Range { start = 5, step = -1, count = 5 }

        it "calculate positive" $
            jotParse (Just 100, Nothing, Just 10, Just 1) `shouldBe`
                Right Range { start = -89, step = 1, count = 100 }

        it "calculate positive large step" $
            jotParse (Just 100, Nothing, Just 10, Just 5) `shouldBe`
                Right Range { start = -485, step = 5, count = 100 }

        it "calculate negative" $
            jotParse (Just 100, Nothing, Just 10, Just (-1)) `shouldBe`
                Right Range { start = 109, step = -1, count = 100 }

        it "calculate negative large step" $
            jotParse (Just 100, Nothing, Just 10, Just (-5)) `shouldBe`
                Right Range { start = 505, step = -5, count = 100 }

    describe "missing upper" $ do
        it "normal" $
            jotParse (Just 5, Just 1, Nothing, Just 1) `shouldBe`
                Right Range { start = 1, step = 1, count = 5 }

        it "negative" $
            jotParse (Just 10, Just 100, Nothing, Just (-1)) `shouldBe`
                Right Range { start = 100, step = -1, count = 10 }

        it "calculate positive" $
            jotParse (Just 100, Just 1, Nothing, Just 1) `shouldBe`
                Right Range { start = 1, step = 1, count = 100 }

        it "calculate negative" $
            jotParse (Just 100, Just 1, Nothing, Just 1) `shouldBe`
                Right Range { start = 1, step = 1, count = 100 }

    describe "missing step" $ do
        it "normal" $
            jotParse (Just 5, Just 1, Just 100, Nothing) `shouldBe`
                Right Range { start = 1, step = 1, count = 5 }

        it "negative" $
            jotParse (Just 10, Just 100, Just 1, Nothing) `shouldBe`
                Right Range { start = 100, step = -1, count = 10 }

        it "calculate positive" $
            jotParse (Just 100, Just 1, Just 10, Nothing) `shouldBe`
                Right Range { start = 1, step = 1, count = 10 }

        it "calculate negative" $
            jotParse (Just 100, Just 1, Just 10, Nothing) `shouldBe`
                Right Range { start = 1, step = 1, count = 10 }

missingTwo :: Spec
missingTwo =
    describe "missing step and" $ do
        it "count" $ do
            jotParse (Nothing, Just 1, Just 100, Nothing) `shouldBe`
                Right Range { start = 1, step = 1, count = 100 }

        it "lower" $ do
            jotParse (Just 5, Nothing, Just 100, Nothing) `shouldBe`
                Right Range { start = 96, step = 1, count = 5 }

            jotParse (Just 100, Nothing, Just 5, Nothing) `shouldBe`
                Right Range { start = -94, step = 1, count = 100 }

        it "upper" $
            jotParse (Just 5, Just 1, Nothing, Nothing) `shouldBe`
                Right Range { start = 1, step = 1, count = 5 }

missingThree :: Spec
missingThree = do
    describe "missing all but" $ do
        it "count" $
            jotParse (Just 5, Nothing, Nothing, Nothing) `shouldBe`
                Right Range { start = 1, step = 1, count = 5 }

        it "lower" $
            jotParse (Nothing, Just 5, Nothing, Nothing) `shouldBe`
                Right Range { start = 5, step = 1, count = 100 }

        it "upper" $ do
            jotParse (Nothing, Nothing, Just 5, Nothing) `shouldBe`
                Right Range { start = -94, step = 1, count = 100 }

            jotParse (Nothing, Nothing, Just 100, Nothing) `shouldBe`
                Right Range { start = 1, step = 1, count = 100 }

            jotParse (Nothing, Nothing, Just 200, Nothing) `shouldBe`
                Right Range { start = 101, step = 1, count = 100 }

        it "step" $ do
            jotParse (Nothing, Nothing, Nothing, Just 5) `shouldBe`
                Right Range { start = 1, step = 5, count = 100 }

            jotParse (Nothing, Nothing, Nothing, Just (-5)) `shouldBe`
                Right Range { start = 1, step = -5, count = 100 }

generalParsing :: Spec
generalParsing = do
    describe "parse numbers" $ do
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

    describe "parse maybe" $
        it "works" $ do
            parseMaybe parseInteger "123" `shouldBe` Right (Just 123)
            parseMaybe parseInteger ""    `shouldBe` Right Nothing
            parseMaybe parseInteger "-"   `shouldBe` Right Nothing
            parseMaybe parseInteger "abc" `shouldBe` Left "abc is not an integer"

    describe "parse range" $ do
        it "works" $ do
            parseRange [] `shouldSatisfy` isLeft

            parseRange ["5"] `shouldBe`
                Right Range { start = 1, step = 1, count = 5 }

            parseRange ["5", "10"] `shouldBe`
                Right Range { start = 10, step = 1, count = 5 }

            parseRange ["5", "10", "15"] `shouldBe`
                Right Range { start = 10, step = 1, count = 5 }

            parseRange ["100", "10", "30", "5"] `shouldBe`
                Right Range { start = 10, step = 5, count = 5 }
