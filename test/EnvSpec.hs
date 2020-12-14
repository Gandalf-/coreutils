module EnvSpec where

import Coreutils.Env
import Test.Hspec

spec :: Spec
spec = do
        describe "envParser" $
            it "empty" $
                envParser "" `shouldBe` Nothing

        describe "envParser" $
            it "no equals" $
                envParser "hello" `shouldBe` Nothing

        describe "envParser" $
            it "no key" $
                envParser "=hello" `shouldBe` Nothing

        describe "envParser" $
            it "unset" $
                envParser "hello=" `shouldBe` Just ("hello", "")

        describe "envParser" $
            it "assign" $
                envParser "hello=there" `shouldBe` Just ("hello", "there")

        describe "envParser" $
            it "assign extra equals" $
                envParser "hello=there=foobar==" `shouldBe`
                    Just ("hello", "there=foobar==")


        describe "getRuntime" $
            it "full" $ do
                let expected = Runtime
                        [("a", "b"), ("c", "d")]
                        (Just ("cat", ["hello.txt"]))
                        (defaultOptions { optEmpty = True})
                    result = getRuntime defaultOptions (words "- a=b c=d cat hello.txt")
                result `shouldBe` expected

        describe "getRuntime" $
            it "no command" $ do
                let expected = Runtime
                        [("a", "b"), ("c", "d")]
                        Nothing
                        (defaultOptions { optEmpty = True})
                    result = getRuntime defaultOptions (words "- a=b c=d")
                result `shouldBe` expected

        describe "getRuntime" $
            it "no sets" $ do
                let expected = Runtime
                        []
                        (Just ("cat", ["hello.txt"]))
                        defaultOptions
                    result = getRuntime defaultOptions (words "cat hello.txt")
                result `shouldBe` expected
