module ShufSpec where

import           Coreutils.Shuf
import           System.Random
import           Test.Hspec

spec :: Spec
spec = do
    describe "simple" $
        it "works" $ do
            simpleShuf g (words "1 2 3") `shouldBe` words "3 2 1"
            simpleShuf g (words "1 2 3 4") `shouldBe` words "4 3 2 1"

    describe "range" $
        it "works" $ do
            let s = RangeShuf 0 5
            shuf s g `shouldBe` words "5 3 4 0 1 2"
    where
        g = mkStdGen 0
