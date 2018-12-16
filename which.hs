module Main where

-- which
--
-- look for the arguments on $PATH

import           Control.Monad
import           Data.List.Split    (splitOn)
import           Data.Maybe
import           System.Directory
import           System.Environment
import           System.Exit


main :: IO ()
-- ^ print the path to each argument if possible
-- if anything didn't exist, exit failure
main = do
        paths <- getArgs >>= mapM which
        mapM_ putStrLn $ catMaybes paths
        when (any isNothing paths) exitFailure


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
        onlyPathsWithFile = filterM (fileInDirectory file)
        onlyExecutables   = filterM runnable
        addFileToPath     = mapM (\c -> return $ c ++ "/" ++ file)


getPaths :: IO [FilePath]
-- ^ convert the PATH variable to a list of valid directories
getPaths = splitOn ":" <$> getEnv "PATH" >>= filterM doesDirectoryExist


runnable :: FilePath -> IO Bool
-- ^ is this file executable? expects it to exist
runnable file = executable <$> getPermissions file


fileInDirectory :: String -> FilePath -> IO Bool
-- ^ is this file in this directory?
fileInDirectory file path = elem file <$> listDirectory path
