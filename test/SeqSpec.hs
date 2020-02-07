module SeqSpec where

import Coreutils.Seq
import Test.Hspec

spec :: Spec
spec = do
        -- happy path
        describe "seq" $
            it "end only" $
                runSeq [5] `shouldBe` Just [1..5]

        describe "seq" $
            it "start end" $
                runSeq [3, 10] `shouldBe` Just [3..10]

        describe "seq" $
            it "start step end" $
                runSeq [3, 3, 10] `shouldBe` Just [3, 6..10]

        -- errors
        describe "seq" $
            it "invalid 1" $
                runSeq [] `shouldBe` Nothing

        describe "seq" $
            it "invalid 2" $
                runSeq [1, 2, 3, 4] `shouldBe` Nothing
