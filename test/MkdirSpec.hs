module MkdirSpec where

import Data.Bits
import System.Directory
import Coreutils.Mkdir
import Test.Hspec
import Data.Either

spec :: Spec
spec = do
    describe "bits" $
        it "works" $ do
            let seven = 7 :: Int
            testBit seven 0 `shouldBe` True
            testBit seven 1 `shouldBe` True
            testBit seven 2 `shouldBe` True

    describe "parse" $ do
        it "int" $ do
            parseMode "123" `shouldBe` Right (1, 2, 3)
            parseMode "723" `shouldBe` Right (7, 2, 3)
            parseMode "700" `shouldBe` Right (7, 0, 0)
            parseMode "013" `shouldBe` Right (0, 1, 3)

            parseMode "823"  `shouldSatisfy` isLeft
            parseMode "junk" `shouldSatisfy` isLeft
            parseMode ""     `shouldSatisfy` isLeft

        it "permssions" $ do
            rwx (parsePerms 1) `shouldBe` (False, False, True)
            rwx (parsePerms 2) `shouldBe` (False, True,  False)
            rwx (parsePerms 3) `shouldBe` (False, True,  True)
            rwx (parsePerms 4) `shouldBe` (True,  False, False)
            rwx (parsePerms 5) `shouldBe` (True,  False, True)
            rwx (parsePerms 6) `shouldBe` (True,  True,  False)
            rwx (parsePerms 7) `shouldBe` (True,  True,  True)
    where
        rwx p = (readable p, writable p, searchable p)
