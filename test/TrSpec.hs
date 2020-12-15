{-# LANGUAGE OverloadedStrings #-}

module TrSpec where

import qualified Data.ByteString.Char8 as C
import           Coreutils.Tr

import           Test.Hspec

spec :: Spec
spec = do
        describe "parse" $
            it "works" $ do
                parse "a-e" `shouldBe` Just "abcde"
                parse "a*5" `shouldBe` Just "aaaaa"

                parse "a*hello" `shouldBe` Nothing
                parse "a*" `shouldBe` Nothing

        describe "interpret" $
            it "works" $ do
                interpret "\\\\hello there" `shouldBe` "\\hello there"
                interpret "\\\\hello \\rthere" `shouldBe` "\\hello \rthere"

        describe "squeeze" $
            it "works" $ do
                setSqueeze "hhello" `shouldBe` "helo"
                setSqueeze "unique" `shouldBe` "unique"
                setSqueeze "ababab" `shouldBe` "ababab"

        describe "complement" $
            it "works" $ do
                setComplement "abcd" `shouldNotContain` "abcd"
                setComplement "  aa  bb" `shouldNotContain` " ab"

        describe "truncate" $
            it "works" $ do
                setTruncate "abcd" "1" `shouldBe` "a"
                setTruncate "abcd" "1234" `shouldBe` "abcd"
                setTruncate "a" "1234" `shouldBe` "a"

        describe "delete" $
            it "works" $ do
                let f = trDelete "1234"
                runDirect f "hello" `shouldBe` "hello"
                runDirect f "hello123" `shouldBe` "hello"
                runDirect f "123" `shouldBe` ""

        describe "translate" $
            it "all to one" $ do
                let f = trTranslate "1234" "a"
                runDirect f "hello" `shouldBe` "hello"
                runDirect f "hello123" `shouldBe` "helloaaa"
                runDirect f "123" `shouldBe` "aaa"

        describe "translate" $
            it "all to two" $ do
                let f = trTranslate "1234" "ab"
                runDirect f "hello" `shouldBe` "hello"
                runDirect f "hello123" `shouldBe` "helloabb"
                runDirect f "123" `shouldBe` "abb"

        describe "tr" $
            it "translate" $ do
                run oTrans "1234" "a"
                    "hello123" "helloaaa"

                run oTrans "[:digit:]" "a"
                    "hello123" "helloaaa"

                run oTrans "[:lower:]" "[:upper:]"
                    "hello there!" "HELLO THERE!"

                run oTrans "[:space:]" "-"
                    "hello there!" "hello-there!"

        describe "tr" $
            it "delete" $ do
                run oDelete "1234" ""
                    "hello123" "hello"

                run oDelete "[:lower:]" ""
                    "hello123" "123"

                run oDelete "[:graph:]" ""
                    "hello123" ""
    where
        oTrans = defaultOptions
        oDelete = defaultOptions { optAction = Delete }



exe :: Either String TrFunc
    -> C.ByteString
    -> Either String C.ByteString
exe func arg = do
    f <- func
    pure $ f arg

run :: Options -> Set -> Set
    -> C.ByteString -> C.ByteString
    -> Expectation
run o s1 s2 b a = do
        let f = tr o s1 s2
        exe f b `shouldBe` Right a

runDirect :: TrFunc -> C.ByteString -> C.ByteString
runDirect f = f
