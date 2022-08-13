{-# LANGUAGE OverloadedStrings #-}

module NlSpec where

import           Coreutils.Nl
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Either
import qualified Streaming.ByteString  as Q

import           Test.Hspec

spec :: Spec
spec = do
    describe "parsing" $ do
        it "getFormat" $ do
            getFormat "ln" `shouldBe` Right LeftNoZeros
            getFormat "rn" `shouldBe` Right RightNoZeros
            getFormat "rz" `shouldBe` Right RightZeros
            getFormat "??" `shouldSatisfy` isLeft

        it "getStyle" $ do
            getStyle "a" `shouldBe` Right AllLines
            getStyle "t" `shouldBe` Right NonEmptyLines
            getStyle "n" `shouldBe` Right NoLines
            getStyle "pA.*B" `shouldBe` Right (RegexLines "A.*B")
            getStyle "??" `shouldSatisfy` isLeft

        it "getNumber" $ do
            getNumber "23938" `shouldBe` Right 23938
            getNumber "hello" `shouldSatisfy` isLeft

    describe "format" $ do
        it "left pad" $ do
            format LeftNoZeros 6 99 `shouldBe` "99    "
            format LeftNoZeros 0 99 `shouldBe` "99"

        it "right pad" $ do
            format RightNoZeros 6 99 `shouldBe` "    99"
            format RightNoZeros 0 99 `shouldBe` "99"

        it "right pad zeros" $ do
            format RightZeros 6 99 `shouldBe` "000099"
            format RightZeros 0 99 `shouldBe` "99"

    describe "match" $
        it "works" $ do
            match AllLines      "???" `shouldBe` True
            match NonEmptyLines "???" `shouldBe` True
            match NonEmptyLines ""    `shouldBe` False
            match NoLines       "??"  `shouldBe` False

            match (RegexLines "a.*e") "apple" `shouldBe` True
            match (RegexLines "a.*e") "blueberry" `shouldBe` False

    describe "getRuntime" $ do
        it "increments" $ do
            let rt1 = getRuntime defaultOptions
            increment rt1 5 `shouldBe` 6

            let rt2 = getRuntime defaultOptions { optLineIncrement = 100 }
            increment rt2 5 `shouldBe` 105

        it "numbers" $ do
            let rt1 = getRuntime defaultOptions
            number rt1 34 `shouldBe` "    34\t"

            let rt2 = getRuntime defaultOptions { optNumberSeparator = "::" }
            number rt2 34 `shouldBe` "    34::"

            let rt3 = getRuntime defaultOptions { optNumberFormat = RightZeros }
            number rt3 34 `shouldBe` "000034\t"

        it "selects" $ do
            let rt1 = getRuntime defaultOptions { optFooterNumbering = NonEmptyLines }
            select rt1 Header "data" `shouldBe` False
            select rt1 Body   "data" `shouldBe` True
            select rt1 Footer "data" `shouldBe` True
            select rt1 Footer ""     `shouldBe` False

        it "sections" $ do
            let rt = getRuntime defaultOptions { optSectionDelimiter = "!" }
            section rt "!!!" `shouldBe` Just Header
            section rt "!!" `shouldBe` Just Body
            section rt "!" `shouldBe` Just Footer

            section rt "hello" `shouldBe` Nothing
            section rt "! hello" `shouldBe` Nothing
            section rt "!!!!" `shouldBe` Nothing

    describe "execute" $ do
        it "works" $ do
            let (st, s) = execute dState "hello"
            position st `shouldBe` Body
            value st `shouldBe` 2
            blanks st `shouldBe` 0
            s `shouldBe` "     1\thello"

        it "uses select" $ do
            let rt = getRuntime defaultOptions { optBodyNumbering = NoLines }
            let (st, s) = execute (getState rt) "hello"
            position st `shouldBe` Body
            value st `shouldBe` 1
            blanks st `shouldBe` 0
            s `shouldBe` "       hello"

        it "counts blanks" $ do
            let (st1, _) = execute dState ""
            blanks st1 `shouldBe` 1

            let (st2, _) = execute dState "hello"
            blanks st2 `shouldBe` 0

        it "skips blanks" $ do
            let rt = getRuntime defaultOptions {
                optBodyNumbering = AllLines, optJoinBlankLines = 2
            }
            let (st1, s1) = execute (getState rt) ""
            blanks st1 `shouldBe` 1
            value  st1 `shouldBe` 1
            s1 `shouldBe` "       "

            let (st2, s2) = execute st1 ""
            blanks st2 `shouldBe` 2
            value  st2 `shouldBe` 2
            s2 `shouldBe` "     1\t"

            let (st3, s3) = execute st2 "hello"
            blanks st3 `shouldBe` 0
            value  st3 `shouldBe` 3
            s3 `shouldBe` "     2\thello"

        it "sections" $ do
            let rt = getRuntime defaultOptions { optSectionDelimiter = "!" }
            let (st1, s1) = execute (getState rt) "!!!"
            position st1 `shouldBe` Header
            s1 `shouldBe` "       "

            let (st2, s2) = execute st1 "hello"
            position st2 `shouldBe` Header
            s2 `shouldBe` "       hello"

            let (st3, s3) = execute st2 "!!"
            position st3 `shouldBe` Body
            s3 `shouldBe` "       "

    describe "io" $
        it "works" $ do
            (t, st) <- run dState "a\nb\nc\n"
            B.lines t `shouldBe` ["     1\ta", "     2\tb", "     3\tc"]
            value st `shouldBe` 4

    where
        dRuntime = getRuntime defaultOptions
        dState = getState dRuntime


run :: NlState -> ByteString -> IO (ByteString, NlState)
run st s = worker Q.toStrict_ (Q.fromStrict s) st
