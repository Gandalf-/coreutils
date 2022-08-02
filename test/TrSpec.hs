{-# LANGUAGE OverloadedStrings #-}

module TrSpec where

import           Coreutils.Tr
import           Data.Array
import           Data.ByteString.Char8 (ByteString)
import           Data.Either
import           Data.Word8
import qualified Streaming.ByteString  as Q

import           Test.Hspec

spec :: Spec
spec = do
    describe "translationTable" $ do
        it "works" $ do
            let (Translator t) = translationTable False "abc" "ABC"
            t ! _a `shouldBe` _A
            t ! _b `shouldBe` _B
            t ! _c `shouldBe` _C
            t ! _d `shouldBe` _d

        it "complement" $ do
            let (Translator t) = translationTable True "abc" "ABC"
            t ! _a `shouldBe` _a
            t ! _0 `shouldBe` _C
            t ! _1 `shouldBe` _C

    describe "translate" $
        it "works" $
            rt upperCase "abc123" `shouldReturn` "ABC123"

        {- TODO, what should this do?
        it "complement" $
            rt cUpperCase "abc123" `shouldReturn` "abcZZZ"
        -}

    describe "deletionTable" $ do
        it "works" $ do
            let (Deleter t) = deletionTable False "123"
            t ! _1 `shouldBe` False
            t ! _2 `shouldBe` False
            t ! _3 `shouldBe` False
            t ! _a `shouldBe` True

        it "complement" $ do
            let (Deleter t) = deletionTable True "123"
            t ! _1 `shouldBe` True
            t ! _a `shouldBe` False
            t ! _b `shouldBe` False

    describe "delete" $ do
        it "works" $
            rt deleteNums "hello123" `shouldReturn` "hello"

        it "complement" $
            rt cDeleteNums "hello123" `shouldReturn` "123"

    describe "squeeze" $
        it "works" $ do
            squeeze "hello" `shouldBe` "helo"
            squeeze "12345" `shouldBe` "12345"
            squeeze "" `shouldBe` ""

    describe "truncate" $
        it "works" $ do
            truncate' "abc" "hello" `shouldBe` "hel"
            truncate' "hello" "abc" `shouldBe` "abc"

    describe "prepare" $ do
        it "invalid" $ do
            prepare opts []
                `shouldBe` Left "At least one set must be provided"
            prepare opts { optAction = Delete } ["1", "2"]
                `shouldBe` Left "Deletion requires one set"
            prepare opts { optAction = Translate } ["1"]
                `shouldBe` Left "Translation requires two sets"

            prepare opts { optAction = Translate } ["1", "2", "3"]
                `shouldSatisfy` isLeft

        it "default translator" $ do
            let (Right e) = prepare opts ["a", "1"]
            rt e "abc" `shouldReturn` "1bc"

        it "default deleter" $ do
            let (Right e) = prepare opts { optAction = Delete } ["123"]
            rt e "abc123" `shouldReturn` "abc"

        it "squeeze first" $ do
            -- Hmm, not really testing much here
            let (Right e) = prepare opts { optSqueeze = True, optAction = Delete } ["aabbc"]
            rt e "abc123" `shouldReturn` "123"

        it "squeeze second" $ do
            let (Right e) = prepare opts { optSqueeze = True } ["123", "aabbc"]
            rt e "abc123" `shouldReturn` "abcabc"
    where
        opts = defaultOptions
        lower = parse "[:lower:]"
        upper = parse "[:upper:]"
        digit = parse "[:digit:]"

        upperCase = translationTable False lower upper
        deleteNums = deletionTable False digit

        -- cUpperCase = translationTable True lower upper
        cDeleteNums = deletionTable True digit

rt :: Translator -> ByteString -> IO ByteString
rt t = Q.toStrict_ . execute t . Q.fromStrict
