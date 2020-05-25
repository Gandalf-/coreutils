{-# LANGUAGE OverloadedStrings #-}

module RevSpec where

import Coreutils.Rev
import Test.Hspec

spec :: Spec
spec = do
        describe "rev" $
            it "empty" $
                rev "" `shouldBe` ""

        describe "rev" $
            it "empty lines" $
                rev "\n\n\n\n" `shouldBe` "\n\n\n\n"

        describe "rev" $
            it "simple" $
                rev "abc\n" `shouldBe` "cba\n"

        describe "rev" $
            it "multiple lines" $
                rev "abc\n123\nxyz\n" `shouldBe` "cba\n321\nzyx\n"
