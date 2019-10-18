module Coreutils.Which where

-- which
--
-- look for the arguments on $PATH

import           Control.Monad      (filterM, when)
import           Data.List.Split    (splitOn)
import           Data.Maybe
import           System.Directory
import           System.Environment (getEnv)
import           System.Exit        (exitFailure)
import           System.Info        (os)

import           Coreutils.Util

data Which = Which

instance Util Which where
    run _ args = do
        -- ^ print the path to each argument if possible
        -- if anything didn't exist, exit failure
        paths <- mapM which args
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
            >>= mapM    addFileToPath
            >>= filterM doesFileExist
            >>= filterM runnable
    where
        addFileToPath directory = pure $ pathJoin directory file


pathJoin :: FilePath -> String -> FilePath
-- ^ join a directory path and filename, platform aware
-- could have used System.FilePath.Posix (filePath) here
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
