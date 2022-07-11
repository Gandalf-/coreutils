{-# LANGUAGE OverloadedStrings #-}

module AwkSpec where

import           Data.Either
import           Data.Text     (Text)
import           Text.Parsec

import           Coreutils.Awk
import           Test.Hspec

spec :: Spec
spec = do
    parsing
    execution
    inputOutput

inputOutput :: Spec
inputOutput = parallel $
    describe "normalize" $ do
        it "missing values" $
            normalize defaultOptions [] `shouldSatisfy` isLeft

        it "parse error" $ do
            normalize defaultOptions ["???"] `shouldSatisfy` isLeft
            normalize (Options (Just "???") " ") ["file.txt"] `shouldSatisfy` isLeft

        it "works" $ do
            normalize defaultOptions [simpleSrc, "file.txt"]
                `shouldBe` Right (Executable " " [FileRecord "file.txt"] simpleProg)
            normalize defaultOptions [simpleSrc]
                `shouldBe` Right (Executable " " [StdinRecord] simpleProg)
            normalize progInOptions []
                `shouldBe` Right (Executable " " [StdinRecord] simpleProg)
    where
        simpleSrc = "{ print NF }"
        simpleProg = Exec [PrintValue [NumFields]]
        progInOptions = Options (Just simpleSrc) " "

execution :: Spec
execution = parallel $ do
    describe "primitives" $ do
        it "show" $ do
            show (String "a") `shouldBe` "a"
            show (Number 123) `shouldBe` "123"

        it "eq" $ do
            String "a" `shouldBe`    String "a"
            String "a" `shouldNotBe` String "b"
            Number 1   `shouldBe`    Number 1
            Number 1   `shouldNotBe` Number 2

            String "a" `shouldNotBe` Number 1
            Number 1   `shouldNotBe` String "a"
            String "1" `shouldBe`    Number 1
            Number 1   `shouldBe`    String "1"

        it "ord" $ do
            String "a" < String "b" `shouldBe` True
            String "a" > String "b" `shouldBe` False
            Number 1   < Number 2   `shouldBe` True
            Number 2   < Number 1   `shouldBe` False

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
            expa "1 2 3" (Primitive (String "a")) `shouldBe` String "a"
            expa ""      NumFields    `shouldBe` Number 0
            expa "1 2 3" NumFields    `shouldBe` String "3" -- ???
            expa "a b c" (FieldVar 0) `shouldBe` String "a b c"
            expa "a b c" (FieldVar 1) `shouldBe` String "a"
            expa "a b c" (FieldVar 3) `shouldBe` String "c"
            expa "a b c" (FieldVar 4) `shouldBe` String ""

    describe "execute" $ do
        it "action" $ do
            exec PrintAll "apple" `shouldBe` "apple\n"

            exec (PrintValue [FieldVar 1]) "a b c"
                `shouldBe` "a\n"
            exec (PrintValue [FieldVar 2, Primitive (String "!")]) "a b c"
                `shouldBe` "b!\n"
            exec (PrintValue [FieldVar 3, Primitive (String " "), Primitive (String "!")]) "a b c"
                `shouldBe` "c !\n"

        it "program" $ do
            exec NoProgram                      "apple" `shouldBe` ""
            exec (Full (Regex ".*") [PrintAll]) "apple" `shouldBe` "apple\n"
            exec (Grep (Regex ".*"))            "apple" `shouldBe` "apple\n"
            exec (Exec [PrintAll])              "apple" `shouldBe` "apple\n"

    describe "matches" $ do
        it "basics" $ do
            match "/a.*/"     "abc" `shouldBe` True
            match "/z.*/"     "abc" `shouldBe` False
            match "! /z.*/"   "abc" `shouldBe` True
            match "!!! /z.*/" "abc" `shouldBe` True

        it "and" $ do
            match "/a.*/ && /.*/" "abc"  `shouldBe` True
            match "/z.*/ && /.*/" "abc"  `shouldBe` False

        it "or" $ do
            match "/z.*/ || /.*/"  "abc" `shouldBe` True
            match "/z.*/ || /z.*/" "abc" `shouldBe` False

        it "relation strings" $ do
            match "\"a\" == \"a\"" ""    `shouldBe` True
            match "\"a\" != \"a\"" ""    `shouldBe` False
            match "$1 == \"a\""    "a b" `shouldBe` True
            match "$2 == \"a\""    "a b" `shouldBe` False

        it "relation numbers" $ do
            match "1  <  2"  ""      `shouldBe` True
            match "10 <  2"  ""      `shouldBe` False
            match "1  <= 1"  ""      `shouldBe` True
            match "10 <= NF" "a b c" `shouldBe` False

        it "relation number fields" $ do
            match "10 < $1" "2 3 4" `shouldBe` False
            match "10 < $2" "2 30"  `shouldBe` True
            match "$1 < $2" "1 2"   `shouldBe` True
            match "$1 > $2" "1 2"   `shouldBe` False

        it "relation string fields" $ do
            match "\"a\" == $1" "a b c" `shouldBe` True
            match "\"a\" == $2" "a b c" `shouldBe` False

            match "$1 == $2" "a b a" `shouldBe` False
            match "$1 == $8" "a b a" `shouldBe` False
            match "$1 == $3" "a b a" `shouldBe` True

    describe "run" $
        it "works" $ do
            run "{print}" "apple"    `shouldBe` "apple\n"
            run "{print $0}" "apple" `shouldBe` "apple\n"
            run "{print $1}" "apple" `shouldBe` "apple\n"
            run "{print NF}" "apple" `shouldBe` "1\n"
            run "{print $2}" "apple" `shouldBe` "\n"

            run "{print $0, NF; print \"!\"}" "apple" `shouldBe` "apple 1\n!\n"

parsing :: Spec
parsing = parallel $ do
    describe "parse primitive" $ do
        it "quoted strings" $ do
            pRun pPrimitive "\"a\""      `shouldBe` Right (String "a")
            pRun pPrimitive "\"apple \"" `shouldBe` Right (String "apple ")
            pRun pPrimitive "\"123! \""  `shouldBe` Right (String "123! ")
            pRun pPrimitive "\"123\""    `shouldBe` Right (String "123")

        it "numbers" $ do
            pRun pPrimitive "123" `shouldBe` Right (Number 123)
            pRun pPrimitive "0"   `shouldBe` Right (Number 0)

        it "other strings" $ do
            pRun pAny "a"      `shouldBe` Right (String "a")
            pRun pAny "a?"     `shouldBe` Right (String "a?")
            pRun pAny "apple " `shouldBe` Right (String "apple ")
            pRun pAny "!123\"" `shouldBe` Right (String "!123\"")
            pRun pAny "123!\"" `shouldBe` Right (String "123!\"")

    describe "parse values" $ do
        it "fields" $ do
            pRun pValue "$0"  `shouldBe` Right (FieldVar 0)
            pRun pValue "$1"  `shouldBe` Right (FieldVar 1)
            pRun pValue "$10" `shouldBe` Right (FieldVar 10)

        it "num fields" $
            pRun pValue "NF" `shouldBe` Right NumFields

    describe "parse pattern" $ do
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

    describe "parse action" $ do
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

        it "negative" $ do
            pRun pAction "junk"        `shouldSatisfy` isLeft
            pRun pAction "print $boop" `shouldSatisfy` isLeft
            pRun pAction "print $-1"   `shouldSatisfy` isLeft

    describe "parse relation" $ do
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

        it "combined" $ do
            pRun pRelation "1 > NF"   `shouldBe` Right (RelGt (Primitive $ Number 1) NumFields)
            pRun pRelation "NF != $5" `shouldBe` Right (RelNe NumFields (FieldVar 5))

    describe "parse expression" $ do
        it "empty negative" $ do
            pRun pEmptyProgram "a"    `shouldSatisfy` isLeft
            pRun pEmptyProgram "   a" `shouldSatisfy` isLeft

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
            pRun pEmptyProgram ""      `shouldBe` Right NoProgram
            pRun pEmptyProgram "   "   `shouldBe` Right NoProgram
            pRun pEmptyProgram "{ }"   `shouldBe` Right NoProgram
            pRun pEmptyProgram " { } " `shouldBe` Right NoProgram

        it "awk" $ do
            pRun pProgram "/.*/"           `shouldBe` Right (Grep (Regex ".*"))
            pRun pProgram "/.*/ { print }" `shouldBe` Right (Full (Regex ".*") [PrintAll])
            pRun pProgram "{ print }"      `shouldBe` Right (Exec [PrintAll])


pRun :: Parsec Text () a -> Text -> Either ParseError a
pRun p = parse (p <* eof) "test"

exec :: Executor a => a -> Text -> Text
exec a t = execute a (getRecord t)

expa :: Text -> Value -> Primitive
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
