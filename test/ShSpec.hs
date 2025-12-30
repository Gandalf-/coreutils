module ShSpec where

import           Test.Hspec

spec :: Spec
spec = parallel $
    describe "sh" $
        it "works" $
            True `shouldBe` True
