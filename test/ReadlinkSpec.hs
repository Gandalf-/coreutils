module ReadlinkSpec where

import           Control.Monad
import           Coreutils.Readlink
import           Data.Either
import           Data.IORef
import           System.Console.GetOpt
import           System.Directory
import           System.IO.Silently
import           System.IO.Temp
import           Test.Hspec

spec :: Spec
spec = parallel $ do
    describe "options" $ do
        it "default display appends newline" $ do
            (out, _) <- capture $ display defaultRuntime "hello"
            out `shouldBe` "hello\n"

        it "-n suppresses trailing newline" $ do
            let Right rt = parse ["-n"]
            (out, _) <- capture $ display rt "hello"
            out `shouldBe` "hello"

        it "-f applies without error" $
            isRight (parse ["-f"]) `shouldBe` True

        it "-h returns Left with usage" $ do
            let Left msg = parse ["-h"]
            msg `shouldSatisfy` ("readlink" `elem`) . words

    describe "runReadlink" $ do
        it "calls execute then display for each path" $ do
            (rt, calls) <- mockRuntime $ \p -> pure (p <> "-resolved")
            runReadlink rt ["a", "b", "c"]
            calls `shouldReturn`
                ["a-resolved", "b-resolved", "c-resolved"]

        it "handles empty path list" $ do
            (rt, calls) <- mockRuntime pure
            runReadlink rt []
            calls `shouldReturn` []

        it "processes paths in order" $ do
            ref <- newIORef []
            let rt = Runtime
                    { execute = \p -> do
                        modifyIORef ref (++ ["exec:" <> p])
                        pure (p <> "!")
                    , display = \p -> modifyIORef ref (++ ["disp:" <> p])
                    }
            runReadlink rt ["x", "y"]
            readIORef ref `shouldReturn`
                ["exec:x", "disp:x!", "exec:y", "disp:y!"]

    describe "integration" $ do
        it "resolves a symlink" $
            withLink $ \target link -> do
                (out, _) <- capture $ readlinkMain [link]
                out `shouldBe` target <> "\n"

        it "-n suppresses trailing newline" $
            withLink $ \target link -> do
                (out, _) <- capture $ readlinkMain ["-n", link]
                out `shouldBe` target

        it "-f canonicalizes the path" $
            withLink $ \target link -> do
                expected <- canonicalizePath target
                (out, _) <- capture $ readlinkMain ["-f", link]
                out `shouldBe` expected <> "\n"

parse :: [String] -> Either String Runtime
parse args =
        foldM (flip id) defaultRuntime opts
    where
        (opts, _, _) = getOpt RequireOrder optionDesc args

-- | Build a mock Runtime that records displayed values.
-- Returns the Runtime and an action to read the recorded calls.
mockRuntime :: (FilePath -> IO FilePath) -> IO (Runtime, IO [String])
mockRuntime exec = do
    ref <- newIORef []
    let rt = Runtime
            { execute = exec
            , display = \p -> modifyIORef ref (++ [p])
            }
    pure (rt, readIORef ref)

-- | Create a temp directory with a symlink, passing (target, link) paths.
withLink :: (FilePath -> FilePath -> IO a) -> IO a
withLink f =
    withSystemTempDirectory "readlink" $ \dir -> do
        let target = dir <> "/real"
            link   = dir <> "/link"
        writeFile target ""
        createFileLink target link
        f target link
