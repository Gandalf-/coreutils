module EnvSpec where

import Coreutils.Env
import Test.Hspec

spec :: Spec
spec = do
        describe "envParser" $ do
            it "empty" $
                envParser "" `shouldBe` Nothing

            it "no equals" $
                envParser "hello" `shouldBe` Nothing

            it "no key" $
                envParser "=hello" `shouldBe` Nothing

            it "unset" $
                envParser "hello=" `shouldBe` Just ("hello", "")

            it "assign" $
                envParser "hello=there" `shouldBe` Just ("hello", "there")

            it "assign extra equals" $
                envParser "hello=there=foobar==" `shouldBe`
                    Just ("hello", "there=foobar==")

        describe "getRuntime" $ do
            it "full" $ do
                let expected = Runtime
                        [("a", "b"), ("c", "d")]
                        (Just ("cat", ["hello.txt"]))
                        (defaultOptions { optEmpty = True})
                    result = getRuntime defaultOptions (words "- a=b c=d cat hello.txt")
                result `shouldBe` expected

            it "no command" $ do
                let expected = Runtime
                        [("a", "b"), ("c", "d")]
                        Nothing
                        (defaultOptions { optEmpty = True})
                    result = getRuntime defaultOptions (words "- a=b c=d")
                result `shouldBe` expected

            it "no sets" $ do
                let expected = Runtime
                        []
                        (Just ("cat", ["hello.txt"]))
                        defaultOptions
                    result = getRuntime defaultOptions (words "cat hello.txt")
                result `shouldBe` expected
