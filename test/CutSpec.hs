module CutSpec (spec) where

import           Coreutils.Cut

import           Test.Hspec


spec :: Spec
spec = do
        testFieldParse
        testParseExpand
        testExact
        testRange
        testMixed
        testFieldInvert
        testExactInvert
        testRangeInvert

nums :: [Int]
nums = [1..10]

testFieldParse :: Spec
-- ^ fields have quite a few variations and constraints
testFieldParse =
        -- happy path
        describe "parseField" $ do
            it "to here" $
                parseField "-4" `shouldBe` Just (Range 1 4)

            it "here on" $
                parseField "4-" `shouldBe` Just (Range 4 infinity)

            it "exact" $
                parseField "4" `shouldBe` Just (Exact 4)

            it "range" $
                parseField "4-10" `shouldBe` Just (Range 4 10)

            -- exact errors
            it "invalid exact" $
                parseField "0" `shouldBe` Nothing

            -- range errors
            it "invalid range 1" $
                parseField "0-10" `shouldBe` Nothing

            it "invalid range 2" $
                parseField "-0" `shouldBe` Nothing

            it "invalid range 2" $
                parseField "6-3" `shouldBe` Nothing

            -- other errors
            it "junk 1" $
                parseField "junk" `shouldBe` Nothing

            it "junk 2" $
                parseField "4-junk" `shouldBe` Nothing

            it "junk 3" $
                parseField "junk-4" `shouldBe` Nothing

            it "junk 4" $
                parseField "-" `shouldBe` Nothing

testExact :: Spec
-- ^ exact fields are simpler than ranges, but there are still a couple edge cases:
-- redundant, inverted, and out of order need to be checked carefully
testExact =
        describe "exact" $ do
            it "single" $
                cut [Exact 1] nums
                `shouldBe` [1]

            it "multiple" $
                cut [Exact 1, Exact 3, Exact 5] nums
                `shouldBe` [1, 3, 5]

            it "multiple, redundant" $
                cut [Exact 1, Exact 1, Exact 1] nums
                `shouldBe` [1]

            it "start later on" $
                cut [Exact 3] nums
                `shouldBe` [3]

            it "start later on, multiple" $
                cut [Exact 3, Exact 5, Exact 7] nums
                `shouldBe` [3, 5, 7]

            it "ignore irrelevant" $
                cut [Exact 500] nums
                `shouldBe` []


testRange :: Spec
-- ^ ranges are the more complex of the two field types, particularly overlapping
-- ranges. so we take extra care there
testRange =
        describe "range" $ do
            it "single" $
                cut [Range 2 4] nums
                `shouldBe` [2..4]

            it "multiple" $
                cut [Range 2 4, Range 7 8] nums
                `shouldBe` [2, 3, 4, 7, 8]

            -- overlapping and contiguous
            it "multiple contiguous" $
                cut [Range 2 4, Range 5 7, Range 8 10] nums
                `shouldBe` [2..10]

            it "overlapping 1" $
                cut [Range 1 4, Range 2 6] nums
                `shouldBe` [1..6]

            it "overlapping 2" $
                cut [Range 1 8, Range 2 9] nums
                `shouldBe` [1..9]

            it "overlapping 3" $
                cut [Range 1 8, Range 2 9, Range 3 5] nums
                `shouldBe` [1..9]

            it "overlapping 4" $
                cut [Range 1 3, Range 2 4, Range 3 5] nums
                `shouldBe` [1..5]

            it "overlapping 5" $
                cut [Range 1 8, Range 6 9, Range 3 5] nums
                `shouldBe` [1..9]

            it "overlapping 6" $
                cut [Range 1 2, Range 2 3, Range 3 4] nums
                `shouldBe` [1..4]

            -- other
            it "ignore irrelevant" $
                cut [Range 20 99] nums
                `shouldBe` []

            -- infinity
            it "infinity" $
                cut [Range 2 infinity] nums
                `shouldBe` [2..10]

            it "simple and infinity" $
                cut [Range 1 3, Range 7 infinity] nums
                `shouldBe` [1, 2, 3, 7, 8, 9, 10]

            it "infinity and irrelevant" $
                cut [Range 1 infinity, Range 5 100] nums
                `shouldBe` [1..10]

            it "edge infinity" $
                cut [Range 10 infinity] nums
                `shouldBe` [10]


testFieldInvert :: Spec
-- ^ cut complement can use the fast implementation if we can invert the provided fields
testFieldInvert =
        describe "invertField" $ do
            it "exact" $
                fieldInvert (Exact 5) `shouldBe` [Range 1 4, Range 6 infinity]

            it "exact edge" $
                fieldInvert (Exact 1) `shouldBe` [Range 2 infinity]

            it "range" $
                fieldInvert (Range 3 8) `shouldBe` [Range 1 2, Range 9 infinity]

            it "range edge" $
                fieldInvert (Range 1 5) `shouldBe` [Range 6 infinity]

            it "range infinity" $
                fieldInvert (Range 5 infinity) `shouldBe` [Range 1 4]


testMixed :: Spec
-- ^ ranges are the more complex of the two field types, particularly overlapping
-- ranges. so we take extra care there
testMixed =
        describe "mixed" $ do
            it "exact, range" $
                cut [Exact 1, Range 4 6] nums
                `shouldBe` [1, 4, 5, 6]

            it "exact, exact, range" $
                cut [Exact 1, Exact 3, Range 7 8] nums
                `shouldBe` [1, 3, 7, 8]

            it "exact, exact, range infinity" $
                cut [Exact 1, Exact 3, Range 7 infinity] nums
                `shouldBe` [1, 3, 7, 8, 9, 10]

            it "range, exact, exact, range" $
                cut [Range 1 3, Exact 4, Exact 6, Range 9 infinity] nums
                `shouldBe` [1, 2, 3, 4, 6, 9, 10]


testParseExpand :: Spec
-- ^ short flag expansion
testParseExpand =
        describe "flag expand" $ do
            it "one character following" $
                parseExpand "-d " `shouldBe` ["-d", " "]

            it "string following" $
                parseExpand "-c1,2,3,4" `shouldBe` ["-c", "1,2,3,4"]

            it "leave alone 1" $
                parseExpand "--complement" `shouldBe` ["--complement"]

            it "leave alone 2" $
                parseExpand "-s" `shouldBe` ["-s"]


testExactInvert :: Spec
-- ^ cut complement for exact fields
testExactInvert =
        describe "exact invert" $ do
            it "single" $
                cut' [Exact 1] nums
                `shouldBe` [2..10]

            it "multiple" $
                cut' [Exact 1, Exact 3, Exact 5] nums
                `shouldBe` [2, 4] <> [6..10]


testRangeInvert :: Spec
-- ^ cut complement for range fields
testRangeInvert =
        describe "range invert" $ do
            it "single" $
                cut' [Range 3 8] nums
                `shouldBe` [1, 2, 9, 10]

            it "multiple" $
                cut' [Range 1 4, Range 6 10] nums
                `shouldBe` [5]

            it "overlapping" $
                cut' [Range 2 7, Range 6 9] nums
                `shouldBe` [1, 10]
