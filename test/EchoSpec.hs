module EchoSpec where

import           Coreutils.Echo     (Echo (..))
import           Coreutils.Util

import           System.IO.Silently
import           Test.Hspec

spec :: Spec
spec =
    describe "system" $ do
        it "basic" $ do
            (stdout, _) <- capture $ echo ["hello", "world"]
            stdout `shouldBe` "hello world\n"

        it "no newline" $ do
            (stdout, _) <- capture $ echo ["-n", "hello", "world"]
            stdout `shouldBe` "hello world"

        it "bad flag" $ do
            (stdout, _) <- capture $ echo ["-nhello", "world"]
            stdout `shouldBe` "-nhello world\n"
    where
        echo = run Echo
