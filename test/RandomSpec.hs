{-# OPTIONS_GHC -Wno-orphans #-}
module RandomSpec where

import           Coreutils.Random
import           Data.Either
import qualified System.Random    as R

import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary Bounds where
    arbitrary = do
        l <- choose (0, 32767)
        s <- choose (1, 32767)
        h <- choose (l, 32767)
        return $ Bounds l s h

instance Show Runtime where
    show _ = ""

spec :: Spec
spec = do
    describe "random" $ do
        it "step 1" $
            property $ \b seed -> do
                let b' = b { _step = 1 }
                check b' seed

        it "step 2" $
            property $ \b seed -> do
                let b' = b { _step = 2 }
                check b' seed

        it "step > low" $
            property $ \b seed -> do
                let b' = b { _step = _low b + 1 }
                check b' seed

        it "full" $
            property $ \b seed ->
                check b seed

    describe "choice" $
        it "works" $
            property $ \xs seed -> do
                let o = choice xs (R.mkStdGen seed)
                null xs || o `elem` xs

    describe "options" $
        it "works" $ do
            options [] `shouldSatisfy` isRight

            options ["5"] `shouldSatisfy` isRight
            options ["a"] `shouldSatisfy` isLeft

            options ["5", "6"] `shouldSatisfy` isRight
            options ["5", "?"] `shouldSatisfy` isLeft

            options ["5", "6", "7"] `shouldSatisfy` isRight
            options ["5", "6", "?"] `shouldSatisfy` isLeft

            options ["choice"] `shouldSatisfy` isLeft
            options ["choice", "a"] `shouldSatisfy` isRight

            options ["1", "2", "3", "4"] `shouldSatisfy` isLeft
    where
        check b seed = do
            let (Bounds l s h) = b
            let o = random b (R.mkStdGen seed)

            let inRange = o >= l && o <= h
            let inStep  = mod (o - l) s == 0
            inRange && inStep
