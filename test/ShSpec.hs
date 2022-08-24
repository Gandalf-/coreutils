module ShSpec where

import           Test.Hspec

spec :: Spec
spec =
    describe "sh" $
        it "works" $
            True `shouldBe` True
