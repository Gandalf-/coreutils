module RealpathSpec where

import           Coreutils.Realpath
import           System.Directory
import           Test.Hspec

spec :: Spec
spec = do
    describe "execute" $ do
        it "works" $ do
            execute cleanRt "foo" `shouldReturn` Right "foo/abc"

        it "errors" $ do
            execute errorRt "foo" `shouldReturn` Left "foo: error"

    describe "runner" $ do
        it "works" $ do
            runner cleanRt []
                `shouldReturn` [Right "/home/abc"]
            runner cleanRt ["1", "2"]
                `shouldReturn` [Right "1/abc", Right "2/abc"]

        it "errors" $ do
            runner errorRt []
                `shouldReturn` [Left "/cwd: error"]
            runner errorRt ["1", "2"]
                `shouldReturn` [Left "1: error", Left "2: error"]

    where
        cleanRt = Runtime {
            defaultPath = pure "/home",
            realpath = \p -> pure $ p <> "/abc",
            onError = const "error"
        }
        errorRt = Runtime {
            defaultPath = pure "/cwd",
            realpath = getSymbolicLinkTarget,
            onError = \p -> head (words $ show p) <> " error"
        }
