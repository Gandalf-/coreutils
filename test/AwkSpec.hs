{-# LANGUAGE OverloadedStrings #-}

module AwkSpec where

import Data.Either
import           Data.Text (Text)
import           Text.Parsec

import Coreutils.Awk
import Test.Hspec

spec :: Spec
spec = do
    parsing
    execution

execution :: Spec
execution = do
    describe "fields" $
        it "works" $ do
            fields "apple"          `shouldBe` ["apple"]
            fields "a b c"          `shouldBe` ["a", "b", "c"]
            fields "apple b c"      `shouldBe` ["apple", "b", "c"]
            fields "  apple b c"    `shouldBe` ["apple", "b", "c"]
            fields "  a   b   c"    `shouldBe` ["a", "b", "c"]
            fields "  a   b   c   " `shouldBe` ["a", "b", "c"]

    describe "records" $
        it "works" $
            getRecord "1 2 3" `shouldBe` Record "1 2 3" ["1", "2", "3"]

    describe "expand" $
        it "works" $ do
            expa "1 2 3" (String "a") `shouldBe` "a"
            expa "1 2 3" Separator    `shouldBe` " "
            expa ""      NumFields    `shouldBe` "0"
            expa "1 2 3" NumFields    `shouldBe` "3"
            expa "a b c" (FieldVar 0) `shouldBe` "a b c"
            expa "a b c" (FieldVar 1) `shouldBe` "a"
            expa "a b c" (FieldVar 3) `shouldBe` "c"
            expa "a b c" (FieldVar 4) `shouldBe` ""

    describe "execute" $ do
        it "action" $ do
            exec PrintAll "apple" `shouldBe` "apple\n"

            exec (PrintValue [FieldVar 1]) "a b c"                        `shouldBe` "a\n"
            exec (PrintValue [FieldVar 2, String "!"]) "a b c"            `shouldBe` "b!\n"
            exec (PrintValue [FieldVar 3, Separator, String "!"]) "a b c" `shouldBe` "c !\n"

        it "program" $ do
            exec NoProgram                      "apple" `shouldBe` ""
            exec (Full (Regex ".*") [PrintAll]) "apple" `shouldBe` "apple\n"
            exec (Grep (Regex ".*"))            "apple" `shouldBe` "apple\n"
            exec (Exec [PrintAll])              "apple" `shouldBe` "apple\n"

    describe "matches" $
        it "works" $ do
            match "/a.*/" "abc"     `shouldBe` True
            match "/z.*/" "abc"     `shouldBe` False
            match "! /z.*/" "abc"   `shouldBe` True
            match "!!! /z.*/" "abc" `shouldBe` True

            match "/a.*/ && /.*/" "abc"  `shouldBe` True
            match "/z.*/ && /.*/" "abc"  `shouldBe` False

            match "/z.*/ || /.*/" "abc"  `shouldBe` True
            match "/z.*/ || /z.*/" "abc" `shouldBe` False

            -- match "\"a\" == \"a\"" "" `shouldBe` True

    describe "run" $
        it "works" $ do
            run "{print}" "apple"    `shouldBe` "apple\n"
            run "{print $0}" "apple" `shouldBe` "apple\n"
            run "{print $1}" "apple" `shouldBe` "apple\n"
            run "{print NF}" "apple" `shouldBe` "1\n"
            run "{print $2}" "apple" `shouldBe` "\n"

            run "{print $0, NF; print \"!\"}" "apple" `shouldBe` "apple 1\n!\n"

parsing :: Spec
parsing = do
    describe "parse values" $ do
        it "strings" $ do
            pRun pValue "\"a\""      `shouldBe` Right (String "a")
            pRun pValue "\"apple \"" `shouldBe` Right (String "apple ")
            pRun pValue "\"123! \""  `shouldBe` Right (String "123! ")

        it "strings negative" $ do
            pRun pValue "\""      `shouldSatisfy` isLeft
            pRun pValue "\"apple" `shouldSatisfy` isLeft

        it "fields" $ do
            pRun pValue "$0"  `shouldBe` Right (FieldVar 0)
            pRun pValue "$1"  `shouldBe` Right (FieldVar 1)
            pRun pValue "$10" `shouldBe` Right (FieldVar 10)

        it "separator" $
            pRun pValue "," `shouldBe` Right Separator

        it "num fields" $
            pRun pValue "NF" `shouldBe` Right NumFields

    describe "parse pattern" $
        it "works" $ do
            pRun pPattern "BEGIN"    `shouldBe` Right Begin
            pRun pPattern "END"      `shouldBe` Right End
            pRun pPattern "/a.*/"    `shouldBe` Right (Regex "a.*")
            pRun pPattern "! /a.*/"  `shouldBe` Right (Not (Regex "a.*"))
            pRun pPattern "!! /a.*/" `shouldBe` Right (Not (Not (Regex "a.*")))

            pRun pPattern "$1 == $2" `shouldBe`
                Right (Relation (RelEqual (FieldVar 1) (FieldVar 2)))
            pRun pPattern "! $1 == $2" `shouldBe`
                Right (Not (Relation (RelEqual (FieldVar 1) (FieldVar 2))))

            pRun pPattern "/a.*/ && /b.*/" `shouldBe`
                Right (And (Regex "a.*") (Regex "b.*"))
            pRun pPattern "/a.*/ && /b.*/ && /c.*/" `shouldBe`
                Right (And (Regex "a.*") (And (Regex "b.*") (Regex "c.*")))
            pRun pPattern "/a.*/ && $1 == $2" `shouldBe`
                Right (And (Regex "a.*") (Relation (RelEqual (FieldVar 1) (FieldVar 2))))

            pRun pPattern "/a.*/ || /b.*/" `shouldBe`
                Right (Or (Regex "a.*") (Regex "b.*"))
            pRun pPattern "/a.*/ || /b.*/ || /c.*/" `shouldBe`
                Right (Or (Regex "a.*") (Or (Regex "b.*") (Regex "c.*")))
            pRun pPattern "/a.*/ || $1 == $2" `shouldBe`
                Right (Or (Regex "a.*") (Relation (RelEqual (FieldVar 1) (FieldVar 2))))

            pRun pPattern "/a.*/ || /b.*/ && /c.*/" `shouldBe`
                Right (Or (Regex "a.*") (And (Regex "b.*") (Regex "c.*")))

            pRun pPattern "! /a.*/ || /b.*/" `shouldBe`
                Right (Not (Or (Regex "a.*") (Regex "b.*")))

            pRun pPattern "! /a.*/ || ! /b.*/" `shouldBe`
                Right (Not (Or (Regex "a.*") (Not (Regex "b.*"))))

    describe "parse action" $ do
        it "works" $ do
            pRun pAction "print $1"    `shouldBe` Right (PrintValue [FieldVar 1])
            pRun pAction "print $1;"   `shouldBe` Right (PrintValue [FieldVar 1])
            pRun pAction "print \"a\"" `shouldBe` Right (PrintValue [String "a"])

            pRun pAction "print $1 $2 ;" `shouldBe`
                Right (PrintValue [FieldVar 1, FieldVar 2])

            pRun pAction "print \"a\" $20" `shouldBe`
                Right (PrintValue [String "a", FieldVar 20])

            pRun pAction "print $1, $2" `shouldBe`
                Right (PrintValue [FieldVar 1, Separator, FieldVar 2])

            pRun pAction "print $1, print" `shouldSatisfy` isLeft
            pRun pAction ";" `shouldSatisfy` isLeft

        it "negative" $ do
            pRun pAction "junk"        `shouldSatisfy` isLeft
            pRun pAction "print $boop" `shouldSatisfy` isLeft
            pRun pAction "print $-1"   `shouldSatisfy` isLeft

    describe "parse relation" $
        it "works" $ do
            pRun pRelation "$1 == $2" `shouldBe` Right (RelEqual (FieldVar 1) (FieldVar 2))
            pRun pRelation "$1 != $2" `shouldBe` Right (RelNotEq (FieldVar 1) (FieldVar 2))
            pRun pRelation "$1 <  $2" `shouldBe` Right (RelLt    (FieldVar 1) (FieldVar 2))
            pRun pRelation "$1 <= $2" `shouldBe` Right (RelLe    (FieldVar 1) (FieldVar 2))
            pRun pRelation "$1 >  $2" `shouldBe` Right (RelGt    (FieldVar 1) (FieldVar 2))
            pRun pRelation "$1 >= $2" `shouldBe` Right (RelGe    (FieldVar 1) (FieldVar 2))

    describe "parse expression" $ do
        it "empty negative" $ do
            pRun pEmpty "a"    `shouldSatisfy` isLeft
            pRun pEmpty "   a" `shouldSatisfy` isLeft

        it "expr" $ do
            pRun pExpr "{ print }" `shouldBe` Right (ActionExpr [PrintAll])
            pRun pExpr "{ print $1 }" `shouldBe`
                Right (ActionExpr [PrintValue [FieldVar 1]])
            pRun pExpr " { print $1 $2} " `shouldBe`
                Right (ActionExpr [PrintValue [FieldVar 1, FieldVar 2]])

            pRun pExpr "{ print $1; print $2 }" `shouldBe`
                Right (ActionExpr [PrintValue [FieldVar 1], PrintValue [FieldVar 2]])
            pRun pExpr "{ print $1; print $2; }" `shouldBe`
                Right (ActionExpr [PrintValue [FieldVar 1], PrintValue [FieldVar 2]])

    describe "parse program" $ do
        it "empty" $ do
            pRun pEmpty ""      `shouldBe` Right NoProgram
            pRun pEmpty "   "   `shouldBe` Right NoProgram
            pRun pEmpty "{ }"   `shouldBe` Right NoProgram
            pRun pEmpty " { } " `shouldBe` Right NoProgram

        it "awk" $ do
            pRun pProgram "/.*/"           `shouldBe` Right (Grep (Regex ".*"))
            pRun pProgram "/.*/ { print }" `shouldBe` Right (Full (Regex ".*") [PrintAll])
            pRun pProgram "{ print }"      `shouldBe` Right (Exec [PrintAll])


pRun :: Parsec Text () a -> Text -> Either ParseError a
pRun p = parse (p <* eof) "test"

exec :: Executor a => a -> Text -> Text
exec a t = execute a (getRecord t)

expa :: Text -> Value -> Text
expa t = expand (getRecord t)

match :: Text -> Text -> Bool
match p r = matches pat (getRecord r)
    where
        pat = case pRun pPattern p of
            (Left _)  -> undefined
            (Right a) -> a

run :: Text -> Text -> Text
run p = exec prog
    where
        prog = case pRun pProgram p of
            (Left _)  -> undefined
            (Right a) -> a
