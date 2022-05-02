{-# LANGUAGE OverloadedStrings #-}
module CmpSpec (spec) where

import Coreutils.Cmp

import qualified Data.ByteString as B
import Numeric
import Control.Monad
import Test.Hspec
import Data.Either
import System.Directory
import System.IO

spec :: Spec
spec = parallel $ do
    describe "getConfig" $ do
        it "error" $ do
            config "" `shouldSatisfy` isLeft
            config "-b" `shouldSatisfy` isLeft
            config "f1 f2 junk" `shouldSatisfy` isLeft
            config "f1 f2 -1" `shouldSatisfy` isLeft
            config "f1 f2 10 junk" `shouldSatisfy` isLeft

        it "variations" $ do
            config "f1" `shouldBe` Right (runBase ("f1", "-"))
            config "f1 f2" `shouldBe` Right (runBase ("f1", "f2"))
            config "f1 f2 10" `shouldBe`
                Right (Config (defaults { optIgnore = (10, 0)}) ("f1", "f2"))
            config "f1 f2 10 20" `shouldBe`
                Right (Config (defaults { optIgnore = (10, 20)}) ("f1", "f2"))

        it "overrides flags" $ do
            config "-i 20 f1 f2 10" `shouldBe`
                Right (Config (defaults { optIgnore = (10, 0)}) ("f1", "f2"))
            config "-i 20:20 f1 f2 10" `shouldBe`
                Right (Config (defaults { optIgnore = (10, 20)}) ("f1", "f2"))
            config "-i 20:20 f1 f2 10 30" `shouldBe`
                Right (Config (defaults { optIgnore = (10, 30)}) ("f1", "f2"))

        it "recognizes flags" $ do
            let _bytes = Config (defaults { optPrintBytes = True}) ("f1", "-")
            config "-b f1" `shouldBe` Right _bytes

            let verbose = Config (defaults { optVolume = Verbose}) ("f1", "-")
            config "-l f1" `shouldBe` Right verbose
            config "--verbose f1" `shouldBe` Right verbose

            let quiet = Config (defaults { optVolume = Quiet}) ("f1", "-")
            config "-s f1" `shouldBe` Right quiet
            config "--quiet f1" `shouldBe` Right quiet
            config "--silent f1" `shouldBe` Right quiet

    describe "parseSkip" $ do
        it "error" $ do
            parseSkip "" `shouldBe` Nothing
            parseSkip "cmp" `shouldBe` Nothing
            parseSkip ":" `shouldBe` Nothing
            parseSkip "3:" `shouldBe` Nothing
            parseSkip ":3" `shouldBe` Nothing

        it "no negatives" $ do
            parseSkip "-3" `shouldBe` Nothing
            parseSkip "-3:-3" `shouldBe` Nothing
            parseSkip "3:-3" `shouldBe` Nothing

        it "single" $ do
            parseSkip "1" `shouldBe` Just (1, 0)
            parseSkip "22" `shouldBe` Just (22, 0)

        it "double" $ do
            parseSkip "1:1" `shouldBe` Just (1, 1)
            parseSkip "22:0" `shouldBe` Just (22, 0)
            parseSkip "2:20" `shouldBe` Just (2, 20)
            parseSkip "002:0020" `shouldBe` Just (2, 20)

        it "single suffix" $ do
            parseSkip "1K" `shouldBe` Just (1024, 0)
            parseSkip "1kB" `shouldBe` Just (1000, 0)

            parseSkip "1M" `shouldBe` Just (1024 ^ 2, 0)
            parseSkip "1mB" `shouldBe` Just (1000 ^ 2, 0)

        it "double suffix" $ do
            parseSkip "1K:34" `shouldBe` Just (1024, 34)
            parseSkip "0:1kB" `shouldBe` Just (0, 1000)

            parseSkip "1K:1M" `shouldBe` Just (1024, 1024 ^ 2)
            parseSkip "1mB" `shouldBe` Just (1000 ^ 2, 0)

    describe "fs tests" $ do
        it "works" $ withTempDir $ do
            writeFile "a.txt" "Hello"
            content <- readFile "a.txt"
            content `shouldBe` "Hello"

        it "skips bytes" $ withTempDir $ do
            r <- setupRuntime "abc" "abc" (defaults { optIgnore = (1, 2) }) >>= applySkips
            d1 <- hGetContents $ _handle $ _lfile r
            d1 `shouldBe` "bc"

            d2 <- hGetContents $ _handle $ _rfile r
            d2 `shouldBe` "c"

    describe "pad" $
        it "works" $ do
            pad SL 1 "1"  `shouldBe` "1"
            pad SL 3 "1"  `shouldBe` "  1"
            pad SL 3 "11" `shouldBe` " 11"
            pad SR 1 "1"  `shouldBe` "1"
            pad SR 3 "1"  `shouldBe` "1  "
            pad SR 3 "11" `shouldBe` "11 "

    describe "byte value" $
        it "octal" $ do
            getByteValue " " `shouldBe` " 40"
            getByteValue "n" `shouldBe` "156"

    describe "byte print" $
        it "cat -t" $ do
            helpBytePrint 40  `shouldBe` " "
            helpBytePrint 112 `shouldBe` "J"
            helpBytePrint 323 `shouldBe` "M-S"
            helpBytePrint 370 `shouldBe` "M-x"
            helpBytePrint   0 `shouldBe` "^@"
            helpBytePrint   4 `shouldBe` "^D"
            helpBytePrint 177 `shouldBe` "^?"

    describe "showByte" $
        it "works" $ do
            showWord False (B.head " ") `shouldBe` " 40"
            showWord False (B.head "n") `shouldBe` "156"

            showWord True (B.head " ") `shouldBe` " 40     "
            showWord True (B.head "n") `shouldBe` "156 n   "
            showWord True 127          `shouldBe` "177 ^?  "

    where
        config = getConfig . words
        runBase = Config defaults

        getByteValue = byteValue . B.head

        helpBytePrint :: Integer -> String
        helpBytePrint i = bytePrint b
            where
                [(b, _)] = readOct $ show i

setupRuntime :: String -> String -> Options -> IO Runtime
setupRuntime d1 d2 opts = do
    writeFile "f1.txt" d1
    writeFile "f2.txt" d2
    getRuntime $ Config opts ("f1.txt", "f2.txt")

withTempDir :: IO a -> IO a
withTempDir s = do
    root <- getTemporaryDirectory
    let target = root <> "/" <> "cmp-test"

    old <- doesDirectoryExist target
    when old $ removeDirectoryRecursive target

    createDirectory target
    withCurrentDirectory target s
