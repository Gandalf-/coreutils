{-# LANGUAGE OverloadedStrings #-}

module RevSpec where

import qualified Data.ByteString.Char8           as C
import qualified Data.ByteString.Streaming.Char8 as Q
import Coreutils.Rev
import Test.Hspec

spec :: Spec
spec =
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
