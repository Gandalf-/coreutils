module Main where

-- which
--
-- look for the arguments on $PATH

import           Control.Monad      (filterM, when)
import           Data.List.Split    (splitOn)
import           Data.Maybe
import           System.Directory
import           System.Environment (getArgs, getEnv)
import           System.Exit        (exitFailure)
import           System.Info        (os)


main :: IO ()
-- ^ print the path to each argument if possible
-- if anything didn't exist, exit failure
main = do
        paths <- getArgs >>= mapM which
        when (any isNothing paths) exitFailure
        mapM_ putStrLn $ catMaybes paths


which :: String -> IO (Maybe String)
-- ^ grab the first result, if there was one
which file =
        maybeHead <$> search file
    where
        maybeHead []    = Nothing
        maybeHead (x:_) = Just x


search :: String -> IO [FilePath]
-- ^ look for a file in the right directories, that's executable
search file =
        getPaths
            >>= onlyPathsWithFile
            >>= addFileToPath
            >>= onlyExecutables
    where
        onlyPathsWithFile = filterM $ fileInDirectory file
        addFileToPath     = mapM (\directory -> pure $ pathJoin directory file)
        onlyExecutables   = filterM runnable


pathJoin :: FilePath -> String -> FilePath
-- ^ join a directory path and filename, platform aware
pathJoin []   file = file
pathJoin base file =
        if last base == separator
            then base <> file
            else base <> [separator] <> file
    where
        separator = case os of
            "mingw32" -> '\\'
            _         -> '/'


getPaths :: IO [FilePath]
-- ^ convert the PATH variable to a list of valid directories
getPaths =
        splitOn separator <$> getEnv "PATH"
            >>= filterM doesDirectoryExist
    where
        separator = case os of
            "mingw32" -> ";"
            _         -> ":"


runnable :: FilePath -> IO Bool
-- ^ is this file executable? expects it to exist
runnable file = executable <$> getPermissions file


fileInDirectory :: String -> FilePath -> IO Bool
-- ^ is this file in this directory?
fileInDirectory file path = elem file <$> listDirectory path
