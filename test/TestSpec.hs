module TestSpec where

import           Coreutils.Test
import           Data.Either

import           Test.Hspec

spec :: Spec
spec = do
    parseSpec
    executeSpec

executeSpec :: Spec
executeSpec = do
        describe "execute pure" $ do
            it "strings" $ do
                run "-n hello" True
                run "a = b" False
                run "a = a" True
                run "a != a" False
                run "a != b" True
                run "-z b" False

            it "nums" $ do
                run "1.0 -eq 1.00" True
                run "1.1 -ne 1" True
                run "2 -le 1" False

        describe "execute io" $ do
            it "files" $ do
                run "-e LICENSE" True
                run "-f LICENSE" True
                run "-d LICENSE" False
                run "-e non-existant" False

            it "directories" $ do
                run "-e src" False
                run "-d src" True

            it "permissions" $ do
                run "-r src" True
                run "-r non-existant" False
                run "-w src" True
                run "-w non-existant" False

            it "file size" $
                run "-s LICENSE" True

        describe "compound" $ do
            it "not" $ do
                run "! -e LICENSE" False
                run "! -f LICENSE" False
                run "! -d LICENSE" True
                run "! ! ! -e non-existant" True

            it "and" $ do
                run "-e LICENSE -a 1 -ge 0" True
                run "-e LICENSE -a 1 -ge 0 -a -n hello" True
                run "-e LICENSE -a 1 -ge 2 -a -n hello" False
                run "-e LICENSE -a 1 -ge 0 -a ! -n hello" False

            it "or" $ do
                run "1 -eq 1 -o 1 -eq 1" True
                run "1 -eq 2 -o 1 -eq 1" True
                run "1 -eq 1 -o 1 -eq 2" True
                run "1 -eq 2 -o 1 -eq 2" False

            it "parens" $ do
                run "(1 -eq 2 -o 1 -eq 1) -a 1 -eq 1" True
                run "(1 -eq 2 -a 1 -eq 1) -a 1 -eq 1" False
                run "! (1 -eq 2 -a 1 -eq 1) -a 1 -eq 1" True

run :: String -> Bool -> Expectation
-- test run that fails on parse errors
run s e =
        case test s of
            (Left p)  -> expectationFailure (show p)
            (Right o) -> o `shouldReturn` e

parseSpec :: Spec
parseSpec = do
        describe "strings" $ do
            it "valid" $ do
                -- TODO can we parse "-z"?
                testParse "-z hello" `shouldBe`
                    Right (Single $ PureOp $ StrLengthZero "hello")

                testParse "a = b" `shouldBe`
                    Right (Single $ PureOp $ StrEqual "a" "b")

                testParse "a  !=  b" `shouldBe`
                    Right (Single $ PureOp $ StrNotEqual "a" "b")

            it "invalid" $
                testParse "a=b" `shouldSatisfy` isLeft

        describe "nums" $ do
            it "valid" $ do
                -- TODO can we parse "-2"?
                testParse "3.4  -eq  5" `shouldBe`
                    Right (Single $ PureOp $ NumEqual 3.4 5.0)

                testParse "3   -gt  0" `shouldBe`
                    Right (Single $ PureOp $ NumGt 3 0)

            it "invalid" $
                testParse "3.4-eq5" `shouldSatisfy` isLeft

        describe "files" $ do
            it "valid" $ do
                testParse "-f file.txt" `shouldBe`
                    Right (Single $ IoOp $ FileRegular "file.txt")

                testParse "-e  file.txt" `shouldBe`
                    Right (Single $ IoOp $ FileExists "file.txt")

            it "invalid" $
                testParse "-ffile.txt" `shouldSatisfy` isLeft

        describe "expression" $ do
            it "parens" $
                testParse "( -f f.txt )" `shouldBe`
                    Right (Sub efile)

            it "double parens" $
                testParse "(( -f f.txt ))" `shouldBe`
                    Right (Sub $ Sub efile)

            it "parens spacing" $
                testParse "(-f f.txt)" `shouldBe`
                    Right (Sub efile)

            it "invalid parens" $ do
                testParse "(-f f.txt" `shouldSatisfy` isLeft
                testParse "-f f.txt )" `shouldSatisfy` isLeft

            it "and" $
                testParse tand `shouldBe`
                    Right eand

            it "double and" $
                testParse (tfile <> " -a " <> tand) `shouldBe`
                    Right (And efile eand)

            it "parens and" $
                testParse "( -f f.txt -a 1 -eq 2) -a -f f.txt"
                    `shouldBe`
                    Right (And (Sub $ And efile enums) efile)

            it "invalid and" $ do
                testParse (tfile <> " -a") `shouldSatisfy` isLeft
                testParse ("-a " <> tfile) `shouldSatisfy` isLeft

            it "or" $
                testParse (tfile <> " -o " <> tnums) `shouldBe`
                    Right (Or efile enums)

            it "invalid or" $ do
                testParse (tfile <> " -o") `shouldSatisfy` isLeft
                testParse ("-o " <> tfile) `shouldSatisfy` isLeft

            it "not" $
                testParse "! -f f.txt" `shouldBe`
                    Right (Not efile)

            it "double not" $
                testParse "! ! -f f.txt" `shouldBe`
                    Right (Not (Not efile))

            it "not and" $
                testParse ("! " <> tand) `shouldBe`
                    Right (Not eand)

            it "and inner not" $
                testParse "-f f.txt -a ! 1 -eq 2" `shouldBe`
                    Right (And efile (Not enums))
    where
        tfile = "-f f.txt"
        efile = Single $ IoOp $ FileRegular "f.txt"

        tnums = "1 -eq 2"
        enums = Single $ PureOp $ NumEqual 1 2

        tand  = tfile <> " -a " <> tnums
        eand  = And efile enums
