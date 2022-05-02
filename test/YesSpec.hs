module YesSpec where

import Coreutils.Yes
import Data.List.Split
import Test.Hspec

spec :: Spec
spec = parallel $
    describe "align" $ do
        it "empty" $
            correct "y"

        it "short string" $
            correct "hello"

        it "long string" $ do
            let long = replicate (block * 2) 'z'
            correct long


correct :: String -> Expectation
correct base = do
        length out `shouldSatisfy` (\l -> l > block - length out)
        chopped `shouldBe` []
    where
        chopped = dropWhile null $ splitOn (base <> "\n") out
        out = align base
