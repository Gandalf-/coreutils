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

        it "hidden files" $ do
            (stdout, _) <- capture $ bname [".bashrc"]
            stdout `shouldBe` unlines [".bashrc"]
            (stdout2, _) <- capture $ bname ["/home/.config", ".config"]
            stdout2 `shouldBe` unlines ["."]

        it "root and slashes" $ do
            (stdout, _) <- capture $ bname ["/"]
            stdout `shouldBe` unlines ["/"]
            (stdout2, _) <- capture $ bname ["////"]
            stdout2 `shouldBe` unlines ["/"]

        it "trailing slashes" $ do
            (stdout, _) <- capture $ bname ["dir/"]
            stdout `shouldBe` unlines ["dir"]
            (stdout2, _) <- capture $ bname ["a/b/c///"]
            stdout2 `shouldBe` unlines ["c"]

        it "dot and dotdot" $ do
            (stdout, _) <- capture $ bname ["."]
            stdout `shouldBe` unlines ["."]
            (stdout2, _) <- capture $ bname [".."]
            stdout2 `shouldBe` unlines [".."]
            (stdout3, _) <- capture $ bname ["../file"]
            stdout3 `shouldBe` unlines ["file"]

        it "multiple extensions" $ do
            (stdout, _) <- capture $ bname ["archive.tar.gz", ".gz"]
            stdout `shouldBe` unlines ["archive.tar"]
            (stdout2, _) <- capture $ bname ["archive.tar.gz", ".tar.gz"]
            stdout2 `shouldBe` unlines ["archive"]

    describe "runner" $ do
        it "invalid arguments" $ do
            runner []              `shouldSatisfy` isLeft
            runner ["a", "b", "c"] `shouldSatisfy` isLeft

        it "basic paths" $ do
            runner ["/a/b/c.jpg"]         `shouldBe` Right "c.jpg"
            runner ["/a/b/c.jpg", ".jpg"] `shouldBe` Right "c"
            runner ["/a/b/c.jpg", ".mp4"] `shouldBe` Right "c.jpg"

        it "hidden files" $ do
            runner ["/home/user/.bashrc"]       `shouldBe` Right ".bashrc"
            runner ["/home/user/.bashrc", ".rc"] `shouldBe` Right ".bash"
            runner [".hidden"]                  `shouldBe` Right ".hidden"

        it "dot and dotdot" $ do
            runner ["."]     `shouldBe` Right "."
            runner [".."]    `shouldBe` Right ".."
            runner ["a/.."]  `shouldBe` Right ".."
            runner ["a/."]   `shouldBe` Right "."

        it "trailing slashes" $ do
            runner ["/a/b/"]     `shouldBe` Right "b"
            runner ["/a/b///"]   `shouldBe` Right "b"
            runner ["/"]         `shouldBe` Right "/"
            runner ["////"]      `shouldBe` Right "/"

        it "complex paths" $ do
            runner ["./file"]          `shouldBe` Right "file"
            runner ["../file"]         `shouldBe` Right "file"
            runner ["a/./b/../c"]      `shouldBe` Right "c"
            runner ["a/////b"]         `shouldBe` Right "b"

        it "special characters" $ do
            runner ["a/hello world"]        `shouldBe` Right "hello world"
            runner ["file-name_123"]        `shouldBe` Right "file-name_123"
            runner ["a/file.tar.gz"]        `shouldBe` Right "file.tar.gz"
            runner ["a/file.tar.gz", ".gz"] `shouldBe` Right "file.tar"

    describe "suffix" $ do
        it "ignores" $ do
            suffix "doop" "boop" `shouldBe` "boop"
            suffix "" "boop"     `shouldBe` "boop"
            suffix ".jpg" ".jpg" `shouldBe` ".jpg"

        it "matches" $ do
            suffix ".jpg" "a.jpg"     `shouldBe` "a"
            suffix ".jpg" "a.jpg.jpg" `shouldBe` "a.jpg"

        it "suffix longer than string" $ do
            suffix ".toolong" "ab"      `shouldBe` "ab"
            suffix "verylongsuffix" "x" `shouldBe` "x"

        it "partial matches" $ do
            suffix ".jpg" "a.jpgx"  `shouldBe` "a.jpgx"
            suffix ".txt" "file.tx" `shouldBe` "file.tx"
            suffix "abc" "xabcd"    `shouldBe` "xabcd"

        it "suffix matches entire basename" $ do
            suffix "file" "file"     `shouldBe` "file"
            suffix "test.txt" "test.txt" `shouldBe` "test.txt"

        it "multiple extensions" $ do
            suffix ".gz" "file.tar.gz"     `shouldBe` "file.tar"
            suffix ".tar.gz" "file.tar.gz" `shouldBe` "file"

        it "hidden files with suffix" $ do
            suffix ".txt" ".hidden.txt" `shouldBe` ".hidden"
            suffix ".rc" ".bashrc"      `shouldBe` ".bash"

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

        it "hidden files" $ do
            basename ".bashrc"      `shouldBe` ".bashrc"
            basename "/home/.bashrc" `shouldBe` ".bashrc"
            basename "a/.hidden"    `shouldBe` ".hidden"
            basename ".hidden/"     `shouldBe` ".hidden"

        it "dot and dotdot" $ do
            basename "."       `shouldBe` "."
            basename "./"      `shouldBe` "."
            basename "./."     `shouldBe` "."
            basename ".."      `shouldBe` ".."
            basename "../"     `shouldBe` ".."
            basename "a/."     `shouldBe` "."
            basename "a/.."    `shouldBe` ".."

        it "paths with dot components" $ do
            basename "./file"    `shouldBe` "file"
            basename "a/./b"     `shouldBe` "b"
            basename "./a/./b"   `shouldBe` "b"
            basename "a/./b/./c" `shouldBe` "c"

        it "paths with dotdot components" $ do
            basename "../file"   `shouldBe` "file"
            basename "a/../b"    `shouldBe` "b"
            basename "a/b/../c"  `shouldBe` "c"

        it "special characters and spaces" $ do
            basename "hello world"     `shouldBe` "hello world"
            basename "a/hello world"   `shouldBe` "hello world"
            basename "hello world/"    `shouldBe` "hello world"
            basename "file-name_123"   `shouldBe` "file-name_123"
            basename "a/file-name_123" `shouldBe` "file-name_123"

        it "multiple dots in filename" $ do
            basename "file.tar.gz"     `shouldBe` "file.tar.gz"
            basename "a/file.tar.gz"   `shouldBe` "file.tar.gz"
            basename ".file.tar.gz"    `shouldBe` ".file.tar.gz"
            basename "a/.file.tar.gz"  `shouldBe` ".file.tar.gz"
