{-# LANGUAGE OverloadedStrings #-}

module SumSpec where

import           Coreutils.Sum
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import           Data.Word
import           Data.Word8
import qualified Streaming.ByteString  as Q
import           System.IO.Silently

import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = parallel $ do
    describe "bsd" $ do
        it "single iteration" $ do
            checksum 0 _a `shouldBe` 97
            checksum (fromIntegral _a) _a `shouldBe` 32913
            checksum (fromIntegral _z) _z `shouldBe` 183

        it "iterates" $ do
            run "abcdef"   `shouldReturn` (2247, 1)
            run "zyxwvu\n" `shouldReturn` (4222, 1)
            run ""         `shouldReturn` (0, 0)

        it "toBlocks" $ do
            toBlocks 18 `shouldBe` 1
            toBlocks 7106100005 `shouldBe` 6939551

        it "non-ASCII characters" $ do
            run (C.pack "こんにちは") `shouldReturn` (53457, 1)
            run (C.pack "Привет")   `shouldReturn` (63604, 1)

        it "long input" $ do
            let longInput = C.replicate 100000 'a'
            run longInput `shouldReturn` (65461, 98)

    describe "display" $
        it "works" $ do
            display "file.txt" 63409 1   `shouldBe` "63409     1 file.txt"
            display "file.txt"  8881 9   `shouldBe` "08881     9 file.txt"
            display "file.txt"  8881 209 `shouldBe` "08881   209 file.txt"
            display ""          8881 209 `shouldBe` "08881   209"

    describe "system" $
        it "works" $ do
            (stdout, _) <- capture $ runSum ["LICENSE", "LICENSE"]
            stdout `shouldBe` unlines ["18673     2 LICENSE", "18673     2 LICENSE"]



run :: ByteString -> IO (Word16, Integer)
run = bsdSum . Q.fromStrict
