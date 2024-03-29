module BasenameSpec where

import           Control.Exception
import           Coreutils.Basename
import           Coreutils.Util
import           Data.Either
import           System.IO.Silently
import           Test.Hspec

-- | https://pubs.opengroup.org/onlinepubs/9699919799/utilities/basename.html

spec :: Spec
spec = parallel $ do
    describe "system" $ do
        let bname = run Basename
        it "basic" $ do
            (stdout, _) <- capture $ bname ["some/file.txt"]
            stdout `shouldBe` unlines ["file.txt"]

        it "suffix" $ do
            (stdout, _) <- capture $ bname ["some/file.txt", ".txt"]
            stdout `shouldBe` unlines ["file"]

    describe "runner" $
        it "works" $ do
            runner []              `shouldSatisfy` isLeft
            runner ["a", "b", "c"] `shouldSatisfy` isLeft

            runner ["/a/b/c.jpg"]         `shouldBe` Right "c.jpg"
            runner ["/a/b/c.jpg", ".jpg"] `shouldBe` Right "c"
            runner ["/a/b/c.jpg", ".mp4"] `shouldBe` Right "c.jpg"

    describe "suffix" $ do
        it "ignores" $ do
            suffix "doop" "boop" `shouldBe` "boop"
            suffix "" "boop"     `shouldBe` "boop"
            suffix ".jpg" ".jpg" `shouldBe` ".jpg"

        it "matches" $ do
            suffix ".jpg" "a.jpg"     `shouldBe` "a"
            suffix ".jpg" "a.jpg.jpg" `shouldBe` "a.jpg"

    describe "basename" $ do
        it "simple" $ do
            basename ""      `shouldBe` ""
            basename "hello" `shouldBe` "hello"

        it "slashes" $ do
            basename "/"    `shouldBe` "/"
            basename "////" `shouldBe` "/"

        it "drop trailing slash" $ do
            basename "hello/"   `shouldBe` "hello"
            basename "hello///" `shouldBe` "hello"

        it "works" $ do
            basename "a/b"     `shouldBe` "b"
            basename "a/b/"    `shouldBe` "b"
            basename "/a/b"    `shouldBe` "b"
            basename "/a/b/"   `shouldBe` "b"
            basename "a/b/c/d" `shouldBe` "d"
            basename "a/////b" `shouldBe` "b"
