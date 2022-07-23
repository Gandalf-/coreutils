{-# LANGUAGE OverloadedStrings #-}

module AwkSpec where

import           Data.Either
import qualified Data.HashMap.Strict as H
import           Data.Text           (Text)
import qualified Data.Text           as T
import           System.IO
import           System.IO.Temp
import           Text.Parsec

import           Coreutils.Awk
import           Test.Hspec

spec :: Spec
spec = do
    parsing
    execution
    inputOutput

inputOutput :: Spec
inputOutput = parallel $ do
    describe "assignOption" $
        it "works" $ do
            assignOption "x=1" defaultOptions
                `shouldBe` Right (defaultOptions { optAssigns = H.fromList [("x", Number 1)]})
            assignOption "x=hi there" defaultOptions
                `shouldBe` Right (defaultOptions { optAssigns = H.fromList [("x", String "hi there")]})

            assignOption "junk" defaultOptions `shouldSatisfy` isLeft
            assignOption "junk=" defaultOptions `shouldSatisfy` isLeft
            assignOption "=junk" defaultOptions `shouldSatisfy` isLeft
            assignOption "=" defaultOptions `shouldSatisfy` isLeft

    describe "normalize" $ do
        it "missing values" $
            normalize defaultOptions [] `shouldSatisfy` isLeft

        it "parse error" $ do
            normalize defaultOptions ["???"] `shouldSatisfy` isLeft
            normalize (Options (Just "???") " " H.empty) ["file.txt"] `shouldSatisfy` isLeft

        it "works" $ do
            normalize defaultOptions [simpleSrc, "file.txt"]
                `shouldBe` Right (Executable emptyState [FileRecord "file.txt"] simpleProg)
            normalize defaultOptions [simpleSrc]
                `shouldBe` Right (Executable emptyState [StdinRecord] simpleProg)
            normalize progInOptions []
                `shouldBe` Right (Executable emptyState [StdinRecord] simpleProg)

    describe "ioExecute" $
        it "works" $ do
            st1 <- ioExecute incRecords (program "$1 > x { x = $1 }") emptyState "3"
            sVariables st1 `shouldBe` H.fromList [("x", Number 3)]
            sRecords st1 `shouldBe` 1

            st2 <- ioExecute incRecords (program "$1 > x { x = $1 }") st1 "1"
            sVariables st2 `shouldBe` H.fromList [("x", Number 3)]
            sRecords st2 `shouldBe` 2

    describe "ioAwk" $ do
        it "simple" $ do
            st <- awk ["1 2 3"] "{ x = $1 }"
            sVariables st `shouldBe` H.fromList [("x", Number 1)]

        it "multi line" $ do
            st <- awk ["1", "3", "2"] "$1 > x { x = $1 }"
            sVariables st `shouldBe` H.fromList [("x", Number 3)]

        it "num records" $ do
            st <- awk ["1", "3", "2", "5"] "{ x = NR }"
            sVariables st `shouldBe` H.fromList [("x", Number 4)]

        it "sorting" $ do
            st <- awk ["1", "3", "5", "2"] "BEGIN { y = 0 }; $1 > x { x = $1 }; END { y = x }"
            H.lookup "y" (sVariables st) `shouldBe` Just (Number 5)
            sRecords st `shouldBe` 4

        it "multiple begin and end" $ do
            st <- awk [] "BEGIN {a = 1}; BEGIN {b = a}; END {c = b}; END {d = c}"
            H.lookup "d" (sVariables st) `shouldBe` Just (Number 1)
            sRecords st `shouldBe` 0
    where
        simpleSrc = "{ print NF }"
        simpleProg = FullProgram [] [Exec [PrintValue [NumFields]]] []
        progInOptions = Options (Just simpleSrc) " " H.empty

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
            tFields " " "apple"          `shouldBe` ["apple"]
            tFields " " "a b c"          `shouldBe` ["a", "b", "c"]
            tFields " " "apple b c"      `shouldBe` ["apple", "b", "c"]
            tFields " " "  apple b c"    `shouldBe` ["apple", "b", "c"]
            tFields " " "  a   b   c"    `shouldBe` ["a", "b", "c"]
            tFields " " "  a   b   c   " `shouldBe` ["a", "b", "c"]

    describe "expand" $ do
        it "works" $ do
            expa "1 2 3" (Primitive (String "a")) `shouldBe` String "a"
            expa ""      NumFields    `shouldBe` Number 0
            expa "1 2 3" NumFields    `shouldBe` String "3" -- ???
            expa "a b c" (FieldVar 0) `shouldBe` String "a b c"
            expa "a b c" (FieldVar 1) `shouldBe` String "a"
            expa "a b c" (FieldVar 3) `shouldBe` String "c"
            expa "a b c" (FieldVar 4) `shouldBe` String ""

        it "reads state" $ do
            expand emptyState { sRecords = 23 } emptyRecord NumRecords
                `shouldBe` Number 23
            expand emptyState emptyRecord (Variable "x")
                `shouldBe` String ""
            expand
                emptyState { sVariables = H.fromList [("x", Number 1)]}
                emptyRecord
                (Variable "x")
                `shouldBe` Number 1

    describe "execute" $ do
        it "action" $ do
            exec PrintAll "apple" `shouldBe` "apple\n"

            exec (program "{ print $1 }") "a b c"
                `shouldBe` "a\n"
            exec (program "{ print $2 \"!\" }") "a b c"
                `shouldBe` "b!\n"
            exec (PrintValue [FieldVar 3, Primitive (String " "), Primitive (String "!")]) "a b c"
                `shouldBe` "c !\n"

            let (st1, _) = execute (Assign "x" NumFields) emptyState "a b c"
            sVariables st1 `shouldBe` H.fromList [("x", Number 3)]

            let (st2, _) = execute (Assign "y" (Variable "x")) st1 "a b c"
            sVariables st2 `shouldBe` H.fromList [("x", Number 3), ("y", Number 3)]

            let (st3, _) = execute (Assign "x" (Primitive (Number 7))) st2 "a b c"
            sVariables st3 `shouldBe` H.fromList [("x", Number 7), ("y", Number 3)]

        it "assign expr" $ do
            let (st1, _) = execute (AssignAdd "x" NumFields) emptyState "a b c"
            H.lookup "x" (sVariables st1) `shouldBe` Just (Number 3)

            let (stm, _) = execute (AssignMul "x" (Primitive (Number 7))) st1 emptyRecord
            H.lookup "x" (sVariables stm) `shouldBe` Just (Number 21)

            let (sts, _) = execute (AssignSub "x" (Primitive (Number 2))) st1 emptyRecord
            H.lookup "x" (sVariables sts) `shouldBe` Just (Number 1)

            let (std, _) = execute (AssignDiv "x" (Primitive (Number 3))) st1 emptyRecord
            H.lookup "x" (sVariables std) `shouldBe` Just (Number 1)

            let (st3, _) = execute (AssignAdd "x" (Primitive (String "a"))) st1 emptyRecord
            H.lookup "x" (sVariables st3) `shouldBe` Just (Number 3)

        it "program" $ do
            exec NoProgram                      "apple" `shouldBe` ""
            exec (Full (Regex ".*") [PrintAll]) "apple" `shouldBe` "apple\n"
            exec (Grep (Regex ".*"))            "apple" `shouldBe` "apple\n"
            exec (Exec [PrintAll])              "apple" `shouldBe` "apple\n"

        it "separator" $ do
            let (st1, _) = execute (program "{x = NF}") (sepState ",") "1,2,3"
            H.lookup "x" (sVariables st1) `shouldBe` Just (Number 3)

            let (st2, _) = execute (program "{x = NF}") (sepState "???") "1???2???3"
            H.lookup "x" (sVariables st2) `shouldBe` Just (Number 3)

        it "fullProgram" $ do
            let fp = FullProgram [program "{x = 1}"] [program "{y = x}"] [program "{z = y}"]
            let (st, _) = execute fp emptyState emptyRecord
            sVariables st `shouldBe` H.fromList [("x", Number 1), ("y", Number 1), ("z", Number 1)]

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
            match "$1 > x"  "1 2"   `shouldBe` True

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
            run "{x = 1; print x}" "" `shouldBe` "1\n"
            run "$1 > x {x = 1; print x; }" "4" `shouldBe` "1\n"
            run "END { print \"a\" }" "" `shouldBe` "a\n"

            run "BEGIN { print NR }" "" `shouldBe` "0\n"
            run "END { print NR }" "" `shouldBe` "0\n"

parsing :: Spec
parsing = parallel $ do
    describe "parse primitive" $ do
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

    describe "parse values" $ do
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

            {-
    describe "expression" $
        it "works" $ do
            let vNum = Val . Primitive . Number
            pRun pExpression "1 + 2"
                `shouldBe` Right (Add (vNum 1) (vNum 2))
            pRun pExpression "1 + 2 + 3"
                `shouldBe` Right (Add (vNum 1) (Add (vNum 2) (vNum 3)))
            -}

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

            pRun pAction "x++"
                `shouldBe` Right (AssignAdd "x" (Primitive (Number 1)))
            pRun pAction "x--"
                `shouldBe` Right (AssignSub "x" (Primitive (Number 1)))

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

        it "variables" $
            pRun pRelation "y > x" `shouldBe` Right (RelGt (Variable "y") (Variable "x"))

        it "combined" $ do
            pRun pRelation "1 > NF"   `shouldBe` Right (RelGt (Primitive $ Number 1) NumFields)
            pRun pRelation "NF != $5" `shouldBe` Right (RelNe NumFields (FieldVar 5))
            pRun pRelation "$1 > x"   `shouldBe` Right (RelGt (FieldVar 1) (Variable "x"))

    describe "parse expression" $ do
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

    describe "parse program" $ do
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



sepState :: Text -> AwkState
sepState s = emptyState { sSeparator = s}

tFields :: Text -> Text -> [Text]
tFields s = fields (sepState s)

pRun :: Parsec Text () a -> Text -> Either ParseError a
pRun p = parse (p <* eof) "test"

program :: Text -> Program
program src = either undefined id $ pRun pProgram src

fProgram :: Text -> FullProgram
fProgram src = either undefined id $ pRun pFullProgram src

exec :: Executor a => a -> Text -> Text
exec a = snd . execute a emptyState

execs :: Executor a => AwkState -> a -> Text -> Text
execs st a = snd . execute a st

expa :: Text -> Value -> Primitive
expa = expand emptyState

match :: Text -> Text -> Bool
match p = matches pat emptyState
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

ioRun :: [String] -> Text -> IO AwkState
ioRun ls src = withSystemTempFile "data.txt" test
    where
        test fname h = do
            hPutStr h $ unlines ls
            hClose h
            let exe = Executable emptyState [FileRecord $ T.pack fname] $ fProgram src
            ioAwk exe

awk :: [Text] -> Text -> IO AwkState
awk ls src = ioAwk exe
    where
        exe = Executable emptyState [TestRecord ls] $ fProgram src
