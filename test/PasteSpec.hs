{-# LANGUAGE OverloadedStrings #-}

module PasteSpec where

import           Control.Monad
import           Coreutils.Paste
import           System.IO
import           Test.Hspec

spec :: Spec
spec = do
    describe "separators" $
        it "works" $ do
            separators "abc"       `shouldBe` ["a", "b", "c"]
            separators "a\\tb"     `shouldBe` ["a", "\t", "b"]
            separators "a\\nb"     `shouldBe` ["a", "\n", "b"]
            separators "a\\\\b"    `shouldBe` ["a", "\\", "b"]
            separators "a\\0b"     `shouldBe` ["a", "", "b"]
            separators "a\\tb\\nc" `shouldBe` ["a", "\t", "b", "\n", "c"]
            separators "a\\0b\\nc" `shouldBe` ["a", "", "b", "\n", "c"]

    describe "getLines" $ do
        it "single handle" $ do
            withFile "LICENSE" ReadMode $ \h -> do
                getLines [h] `shouldReturn` [Just lic1]

        it "multiple handles" $ do
            withFile "LICENSE" ReadMode $ \h1 -> do
                withFile "LICENSE" ReadMode $ \h2 -> do
                    getLines [h1, h2] `shouldReturn` [Just lic1, Just lic1]

        it "eof" $ do
            withFile "LICENSE" ReadMode $ \h -> do
                readUntilEOF h
                getLines [h] `shouldReturn` [Nothing]
                getLines [h] `shouldReturn` [Nothing]

    describe "paste" $ do
        it "works" $ do
            withFile "LICENSE" ReadMode $ \h1 -> do
                withFile "LICENSE" ReadMode $ \h2 -> do
                    ls <- paste [h1, h2] $ repeat "\t"
                    let (l1:l2:l3:_) = ls
                    l1 `shouldBe` lic1 <> "\t" <> lic1
                    l2 `shouldBe` "\t"
                    l3 `shouldBe` lic3 <> "\t" <> lic3

    describe "combine" $
        it "works" $ do
            combine ["a", "b", "c"] (repeat "!")       `shouldBe` ["a!", "b!", "c\n"]
            combine ["a", "b", "c"] (cycle ["1", "2"]) `shouldBe` ["a1", "b2", "c\n"]

    describe "delimit" $
        it "works" $ do
            delimit ["a", "b", "c"] (repeat "!")       `shouldBe` "a!b!c"
            delimit ["a", "b", "c"] (cycle ["1", "2"]) `shouldBe` "a1b2c"
    where
        lic1 = "MIT License"
        lic3 = "Copyright (c) 2018 Austin"


readUntilEOF :: Handle -> IO ()
readUntilEOF h = do
    done <- hIsEOF h
    unless done $ hGetLine h >> readUntilEOF h
