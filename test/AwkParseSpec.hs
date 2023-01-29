{-# LANGUAGE OverloadedStrings #-}

module AwkParseSpec where

import           Data.Either
import           Data.Text     (Text)
import           Text.Parsec

import           Coreutils.Awk
import           Test.Hspec

spec :: Spec
spec = parallel $ do
    describe "primitive" $ do
        it "quoted strings" $ do
            pRun pPrimitive "\"a\""      `shouldBe` Right (String "a")
            pRun pPrimitive "\"apple \"" `shouldBe` Right (String "apple ")
            pRun pPrimitive "\"123! \""  `shouldBe` Right (String "123! ")
            pRun pPrimitive "\"123\""    `shouldBe` Right (String "123")

            pRun pPrimitive " " `shouldSatisfy` isLeft

        it "numbers" $ do
            pRun pPrimitive "123" `shouldBe` Right (Number 123)
            pRun pPrimitive "0"   `shouldBe` Right (Number 0)

        it "other strings" $ do
            pRun pAny "a"      `shouldBe` Right (String "a")
            pRun pAny "a?"     `shouldBe` Right (String "a?")
            pRun pAny "apple " `shouldBe` Right (String "apple ")
            pRun pAny "!123\"" `shouldBe` Right (String "!123\"")
            pRun pAny "123!\"" `shouldBe` Right (String "123!\"")

    describe "values" $ do
        it "fields" $ do
            pRun pValue "$0"  `shouldBe` Right (FieldVar 0)
            pRun pValue "$1"  `shouldBe` Right (FieldVar 1)
            pRun pValue "$10" `shouldBe` Right (FieldVar 10)

        it "num fields" $
            pRun pValue "NF" `shouldBe` Right NumFields

        it "variables" $ do
            pRun pValue " "  `shouldSatisfy` isLeft
            pRun pValue "a?" `shouldSatisfy` isLeft

            pRun pValue "x" `shouldBe` Right (Variable "x")
            pRun pValue "abc2" `shouldBe` Right (Variable "abc2")

    xdescribe "expression" $
        it "works" $ do
            let vNum = Val . Primitive . Number
            pRun pExpression "1 + 2"
                `shouldBe` Right (Add (vNum 1) (vNum 2))
            pRun pExpression "1 + 2 + 3"
                `shouldBe` Right (Add (vNum 1) (Add (vNum 2) (vNum 3)))

    describe "pattern" $ do
        it "basics" $ do
            pRun pPattern "BEGIN"    `shouldBe` Right Begin
            pRun pPattern "END"      `shouldBe` Right End
            pRun pPattern "/a.*/"    `shouldBe` Right (Regex "a.*")
            pRun pPattern "! /a.*/"  `shouldBe` Right (Not (Regex "a.*"))
            pRun pPattern "!! /a.*/" `shouldBe` Right (Not (Not (Regex "a.*")))

        it "relations" $ do
            pRun pPattern "$1 < $2" `shouldBe`
                Right (Relation (RelLt (FieldVar 1) (FieldVar 2)))
            pRun pPattern "$1 == $2" `shouldBe`
                Right (Relation (RelEq (FieldVar 1) (FieldVar 2)))
            pRun pPattern "! $1 == $2" `shouldBe`
                Right (Not (Relation (RelEq (FieldVar 1) (FieldVar 2))))

        it "and" $ do
            pRun pPattern "$1 < $2 && $2 < $3" `shouldBe`
                Right (And
                    (Relation (RelLt (FieldVar 1) (FieldVar 2)))
                    (Relation (RelLt (FieldVar 2) (FieldVar 3))))
            pRun pPattern "/a.*/ && /b.*/" `shouldBe`
                Right (And (Regex "a.*") (Regex "b.*"))
            pRun pPattern "/a.*/ && /b.*/ && /c.*/" `shouldBe`
                Right (And (Regex "a.*") (And (Regex "b.*") (Regex "c.*")))
            pRun pPattern "/a.*/ && $1 == $2" `shouldBe`
                Right (And (Regex "a.*") (Relation (RelEq (FieldVar 1) (FieldVar 2))))

        it "or" $ do
            pRun pPattern "/a.*/ || /b.*/" `shouldBe`
                Right (Or (Regex "a.*") (Regex "b.*"))
            pRun pPattern "/a.*/ || /b.*/ || /c.*/" `shouldBe`
                Right (Or (Regex "a.*") (Or (Regex "b.*") (Regex "c.*")))
            pRun pPattern "/a.*/ || $1 == $2" `shouldBe`
                Right (Or (Regex "a.*") (Relation (RelEq (FieldVar 1) (FieldVar 2))))

        it "complex" $ do
            pRun pPattern "/a.*/ || /b.*/ && /c.*/" `shouldBe`
                Right (Or (Regex "a.*") (And (Regex "b.*") (Regex "c.*")))
            pRun pPattern "! /a.*/ || /b.*/" `shouldBe`
                Right (Not (Or (Regex "a.*") (Regex "b.*")))
            pRun pPattern "! /a.*/ || ! /b.*/" `shouldBe`
                Right (Not (Or (Regex "a.*") (Not (Regex "b.*"))))
            pRun pPattern "$1 < $2 && /pine/" `shouldBe`
                Right (And
                    (Relation (RelLt (FieldVar 1) (FieldVar 2)))
                    (Regex "pine"))

    describe "action" $ do
        it "works" $ do
            pRun pAction "print $1"    `shouldBe` Right (PrintValue [FieldVar 1])
            pRun pAction "print $1;"   `shouldBe` Right (PrintValue [FieldVar 1])
            pRun pAction "print \"a\"" `shouldBe` Right (PrintValue [Primitive (String "a")])

            pRun pAction "print $1 $2 ;" `shouldBe`
                Right (PrintValue [FieldVar 1, FieldVar 2])

            pRun pAction "print \"a\" $20" `shouldBe`
                Right (PrintValue [Primitive (String "a"), FieldVar 20])

            pRun pAction "print $1, $2" `shouldBe`
                Right (PrintValue [FieldVar 1, Primitive (String " "), FieldVar 2])

            pRun pAction "print $1, print" `shouldSatisfy` isLeft
            pRun pAction ";" `shouldSatisfy` isLeft

        it "assignment" $ do
            pRun pAction "x=3"
                `shouldBe` Right (Assign "x" (Primitive (Number 3)))
            pRun pAction "abc = \"hello\""
                `shouldBe` Right (Assign "abc" (Primitive (String "hello")))
            pRun pAction "x = NF"
                `shouldBe` Right (Assign "x" NumFields)
            pRun pAction "x = $1"
                `shouldBe` Right (Assign "x" (FieldVar 1))

        it "expr assign" $ do
            pRun pAction "x += 3"
                `shouldBe` Right (AssignAdd "x" (Primitive (Number 3)))
            pRun pAction "x += \"hello\""
                `shouldBe` Right (AssignAdd "x" (Primitive (String "hello")))
            pRun pAction "x -= 3"
                `shouldBe` Right (AssignSub "x" (Primitive (Number 3)))
            pRun pAction "x *= 3"
                `shouldBe` Right (AssignMul "x" (Primitive (Number 3)))
            pRun pAction "x /= 3"
                `shouldBe` Right (AssignDiv "x" (Primitive (Number 3)))
            pRun pAction "x %= 3"
                `shouldBe` Right (AssignMod "x" (Primitive (Number 3)))
            pRun pAction "x **= 3"
                `shouldBe` Right (AssignExp "x" (Primitive (Number 3)))

            pRun pAction "x++"
                `shouldBe` Right (AssignAdd "x" (Primitive (Number 1)))
            pRun pAction "x--"
                `shouldBe` Right (AssignSub "x" (Primitive (Number 1)))

        it "negative" $ do
            pRun pAction "junk"        `shouldSatisfy` isLeft
            pRun pAction "print $boop" `shouldSatisfy` isLeft
            pRun pAction "print $-1"   `shouldSatisfy` isLeft

    describe "relation" $ do
        it "fields" $ do
            pRun pRelation "$1 == $2" `shouldBe` Right (RelEq (FieldVar 1) (FieldVar 2))
            pRun pRelation "$1 != $2" `shouldBe` Right (RelNe (FieldVar 1) (FieldVar 2))
            pRun pRelation "$1 <  $2" `shouldBe` Right (RelLt (FieldVar 1) (FieldVar 2))
            pRun pRelation "$1 <= $2" `shouldBe` Right (RelLe (FieldVar 1) (FieldVar 2))
            pRun pRelation "$1 >  $2" `shouldBe` Right (RelGt (FieldVar 1) (FieldVar 2))
            pRun pRelation "$1 >= $2" `shouldBe` Right (RelGe (FieldVar 1) (FieldVar 2))

        it "numbers" $ do
            pRun pRelation "1 >  2" `shouldBe` Right (RelGt (Primitive $ Number 1) (Primitive $ Number 2))
            pRun pRelation "1 <= 2" `shouldBe` Right (RelLe (Primitive $ Number 1) (Primitive $ Number 2))

        it "variables" $
            pRun pRelation "y > x" `shouldBe` Right (RelGt (Variable "y") (Variable "x"))

        it "combined" $ do
            pRun pRelation "1 > NF"   `shouldBe` Right (RelGt (Primitive $ Number 1) NumFields)
            pRun pRelation "NF != $5" `shouldBe` Right (RelNe NumFields (FieldVar 5))
            pRun pRelation "$1 > x"   `shouldBe` Right (RelGt (FieldVar 1) (Variable "x"))

    describe "expression" $ do
        it "empty negative" $ do
            pRun pEmptyProgram "a"    `shouldSatisfy` isLeft
            pRun pEmptyProgram "   a" `shouldSatisfy` isLeft

        it "expr" $ do
            pRun pExpr "{ print }" `shouldBe` Right (ActionExpr [PrintAll])
            pRun pExpr "{ print $1 }" `shouldBe`
                Right (ActionExpr [PrintValue [FieldVar 1]])
            pRun pExpr "{ print $1 $2}" `shouldBe`
                Right (ActionExpr [PrintValue [FieldVar 1, FieldVar 2]])

            pRun pExpr "{ print $1; print $2 }" `shouldBe`
                Right (ActionExpr [PrintValue [FieldVar 1], PrintValue [FieldVar 2]])
            pRun pExpr "{ print $1; print $2; }" `shouldBe`
                Right (ActionExpr [PrintValue [FieldVar 1], PrintValue [FieldVar 2]])

    describe "program" $ do
        it "empty" $ do
            pRun pEmptyProgram ""      `shouldBe` Right NoProgram
            pRun pEmptyProgram "   "   `shouldBe` Right NoProgram
            pRun pEmptyProgram "{ }"   `shouldBe` Right NoProgram
            pRun pEmptyProgram " { } " `shouldBe` Right NoProgram

        it "program" $ do
            pRun pProgram "/.*/"           `shouldBe` Right (Grep (Regex ".*"))
            pRun pProgram "/.*/ { print }" `shouldBe` Right (Full (Regex ".*") [PrintAll])
            pRun pProgram "{ print }"      `shouldBe` Right (Exec [PrintAll])

            pRun pProgram "BEGIN { print }" `shouldBe` Right (Full Begin [PrintAll])
            pRun pProgram "BEGIN{ print }"  `shouldBe` Right (Full Begin [PrintAll])
            pRun pProgram "BEGIN{ print}"   `shouldBe` Right (Full Begin [PrintAll])
            pRun pProgram "BEGIN{print}"    `shouldBe` Right (Full Begin [PrintAll])

        it "full program" $ do
            pRun pFullProgram "/a/ {print}; /b/ {print}"
                `shouldBe` Right (
                    FullProgram [] [Full (Regex "a") [PrintAll], Full (Regex "b") [PrintAll]] []
                )

            pRun pFullProgram "BEGIN {print}; /b/ {print}"
                `shouldBe` Right (
                    FullProgram [Full Begin [PrintAll]] [Full (Regex "b") [PrintAll]] []
                )

            pRun pFullProgram "END {print}; /b/ {print}"
                `shouldBe` Right (
                    FullProgram [] [Full (Regex "b") [PrintAll]] [Full End [PrintAll]]
                )

            pRun pFullProgram "END {print}; /b/ {print}"
                `shouldBe` pRun pFullProgram "END{print};/b/{print}"

        it "program separators" $ do
            let base = pRun pFullProgram "END {x=1}; /b/ {x=1}"

            base `shouldBe` pRun pFullProgram "END {x=1} /b/ {x=1}"
            -- base `shouldBe` pRun pFullProgram "END {x=1}/b/ {x=1}"


pRun :: Parsec Text () a -> Text -> Either ParseError a
pRun p = parse (p <* eof) "test"

program :: Text -> Program
program src = either undefined id $ pRun pProgram src

fProgram :: Text -> FullProgram
fProgram src = either undefined id $ pRun pFullProgram src
