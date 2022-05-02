module SeqSpec where

import           Coreutils.Seq
import           Test.Hspec

spec :: Spec
spec = parallel $
    -- happy path
    describe "seq" $ do
        it "end only" $
            runSeq [5 :: Int] `shouldBe` Right [1..5]

        it "start end" $
            runSeq [3 :: Int, 10] `shouldBe` Right [3..10]

        it "start step end" $
            runSeq [3 :: Int, 3, 10] `shouldBe` Right [3, 6..10]

        -- errors
        it "invalid" $ do
            runSeq ([] :: [Int]) `shouldBe` err
            runSeq [1 :: Int, 2, 3, 4] `shouldBe` err
    where
        err = Left "unable to parse arguments as numbers"
