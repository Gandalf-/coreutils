module TestSpec where

import           Coreutils.Test
import           Data.Either

import           Test.Hspec

spec :: Spec
spec = do
        describe "strings" $
            it "works" $ do
                testParse "-z hello" `shouldBe`
                    Right (Single $ StrLengthZero "hello")

                testParse "a = b" `shouldBe`
                    Right (Single $ StrEqual "a" "b")

                testParse "a=b" `shouldSatisfy` isLeft

        describe "nums" $
            it "works" $ do
                testParse "3.4  -eq  5" `shouldBe`
                    Right (Single $ NumEqual 3.4 5.0)

                -- this works, but doesn't in GNU
                testParse "3.4-eq5" `shouldBe`
                    Right (Single $ NumEqual 3.4 5.0)

                testParse "3   -gt  0" `shouldBe`
                    Right (Single $ NumGt 3 0)
{-
        describe "files" $
            it "works" $ do
                testParse "-f file.txt" `shouldBe`
                    Right (Single $ FileRegular "file.txt")

                testParse "-ffile.txt" `shouldSatisfy` isLeft

                testParse "-e  file.txt" `shouldBe`
                    Right (Single $ FileExists "file.txt")
-}
