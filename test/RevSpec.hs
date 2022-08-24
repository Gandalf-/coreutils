{-# LANGUAGE OverloadedStrings #-}

module RevSpec where

import           Coreutils.Rev
import qualified Data.ByteString.Char8 as C
import qualified Streaming.ByteString  as Q
import           Test.Hspec

spec :: Spec
spec = parallel $
    describe "rev" $ do
        it "empty" $
            runRev "" `shouldReturn` ""

        it "empty lines" $
            runRev "\n\n\n\n" `shouldReturn` "\n\n\n\n"

        it "simple" $
            runRev "abc\n" `shouldReturn` "cba\n"

        it "multiple lines" $
            runRev "abc\n123\nxyz\n" `shouldReturn` "cba\n321\nzyx\n"

runRev :: C.ByteString -> IO C.ByteString
runRev = Q.toStrict_ . rev . Q.fromStrict
