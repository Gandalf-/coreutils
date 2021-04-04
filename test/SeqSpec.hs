module SeqSpec where

import           Coreutils.Seq
import           Test.Hspec

spec :: Spec
spec =
        -- happy path
        describe "seq" $ do
            it "end only" $
                runSeq [5] `shouldBe` Right [1..5]

            it "start end" $
                runSeq [3, 10] `shouldBe` Right [3..10]

            it "start step end" $
                runSeq [3, 3, 10] `shouldBe` Right [3, 6..10]

            -- errors
            it "invalid 1" $
                runSeq [] `shouldBe` err

            it "invalid 2" $
                runSeq [1, 2, 3, 4] `shouldBe` err
    where
        err = Left "unable to parse arguments as numbers"
