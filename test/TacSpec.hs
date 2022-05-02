{-# LANGUAGE OverloadedStrings #-}

module TacSpec where

import Coreutils.Tac
import Test.Hspec

spec :: Spec
spec = parallel $
    describe "locations" $ do
        it "zero zero" $
            locations 0 1000 `shouldBe` [(0, 0)]

        it "single smaller block" $
            locations 500 1000 `shouldBe` [(0, 500)]

        it "multiple blocks align" $
            locations 1000 500 `shouldBe` [(500, 500), (0, 500)]

        it "multiple blocks misalign" $
            locations 1020 500 `shouldBe` [(520, 500), (20, 500), (0, 20)]
