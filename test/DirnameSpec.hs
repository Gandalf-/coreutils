module DirnameSpec (spec) where

import           Coreutils.Dirname

import           Test.Hspec

spec :: Spec
spec = parallel $
    describe "posix" $ do
        it "simple cases" $ do
            dirname "/usr/bin" `shouldBe` "/usr"
            dirname "/a/b/c" `shouldBe` "/a/b"
            dirname "/a" `shouldBe` "/"

        it "trailing slashes" $ do
            dirname "/a/b/c////" `shouldBe` "/a/b"
            dirname "/a/////" `shouldBe` "/"
            dirname "/" `shouldBe` "/"

        it "no slashes" $ do
            dirname "stdio.h" `shouldBe` "."
            dirname "blueberry" `shouldBe` "."

        it "non root" $ do
            dirname "a/b/c////" `shouldBe` "a/b"
            dirname "a" `shouldBe` "."
            dirname "a/" `shouldBe` "."
            dirname "" `shouldBe` "."
