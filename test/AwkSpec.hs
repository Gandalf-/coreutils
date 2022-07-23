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
            H.lookup "x" (sVariables st1) `shouldBe` Just (Number 3)
            sRecords st1 `shouldBe` 1

            st2 <- ioExecute incRecords (program "$1 > x { x = $1 }") st1 "1"
            H.lookup "x" (sVariables st2) `shouldBe` Just (Number 3)
            sRecords st2 `shouldBe` 2

    describe "ioAwk" $ do
        it "simple" $ do
            st <- awk ["1 2 3"] "{ x = $1 }"
            H.lookup "x" (sVariables st) `shouldBe` Just (Number 1)

        it "multi line" $ do
            st <- awk ["1", "3", "2"] "$1 > x { x = $1 }"
            H.lookup "x" (sVariables st) `shouldBe` Just (Number 3)

        it "num records" $ do
            st <- awk ["1", "3", "2", "5"] "{ x = NR }"
            H.lookup "x" (sVariables st) `shouldBe` Just (Number 4)

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

            exec (program "{ NF *= 3; print NF; }") "a b c"
                `shouldBe` "9\n"

            let (st1, _) = execute (Assign "x" NumFields) emptyState "a b c"
            H.lookup "x" (sVariables st1) `shouldBe` Just (Number 3)

            let (st2, _) = execute (Assign "y" (Variable "x")) st1 "a b c"
            H.lookup "x" (sVariables st2) `shouldBe` Just (Number 3)
            H.lookup "y" (sVariables st2) `shouldBe` Just (Number 3)

            let (st3, _) = execute (Assign "x" (Primitive (Number 7))) st2 "a b c"
            H.lookup "x" (sVariables st3) `shouldBe` Just (Number 7)
            H.lookup "y" (sVariables st3) `shouldBe` Just (Number 3)

        it "assign expr" $ do
            let (st1, _) = execute (AssignAdd "x" NumFields) emptyState "a b c"
            H.lookup "x" (sVariables st1) `shouldBe` Just (Number 3)

            let (stm, _) = execute (AssignMul "x" (Primitive (Number 7))) st1 emptyRecord
            H.lookup "x" (sVariables stm) `shouldBe` Just (Number 21)

            let (sts, _) = execute (AssignSub "x" (Primitive (Number 2))) st1 emptyRecord
            H.lookup "x" (sVariables sts) `shouldBe` Just (Number 1)

            let (std, _) = execute (AssignDiv "x" (Primitive (Number 3))) st1 emptyRecord
            H.lookup "x" (sVariables std) `shouldBe` Just (Number 1)

            let (sto, _) = execute (AssignMod "x" (Primitive (Number 1))) st1 emptyRecord
            H.lookup "x" (sVariables sto) `shouldBe` Just (Number 0)

            let (ste, _) = execute (AssignExp "x" (Primitive (Number 3))) st1 emptyRecord
            H.lookup "x" (sVariables ste) `shouldBe` Just (Number 27)

            -- string
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
            H.lookup "x" (sVariables st) `shouldBe` Just (Number 1)
            H.lookup "y" (sVariables st) `shouldBe` Just (Number 1)
            H.lookup "z" (sVariables st) `shouldBe` Just (Number 1)

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
