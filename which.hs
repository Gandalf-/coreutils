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


runnable :: FilePath -> IO Bool
-- ^ is this file executable? expects it to exist
runnable path = executable <$> getPermissions path


paths :: IO [FilePath]
-- ^ convert the PATH variable to a list of valid directories
paths = splitOn ":" <$> getEnv "PATH" >>= filterM doesDirectoryExist


find :: String -> FilePath -> IO Bool
-- ^ is this file in this directory?
find file path = elem file <$> listDirectory path


search :: String -> IO [FilePath]
-- ^ look for a file in the right directories, that's executable
search file =
        paths >>= onlyPathsWithFile >>= addFileToPath >>= onlyExecutables
    where
        onlyPathsWithFile = filterM (find file)
        onlyExecutables   = filterM runnable
        addFileToPath     = mapM (\c -> return $ c ++ "/" ++ file)


which :: String -> IO (Maybe String)
-- ^ grab the first result, if there was one
which file =
        maybeHead <$> search file
    where
        maybeHead []    = Nothing
        maybeHead (x:_) = Just x


main :: IO ()
-- ^ print the path to each argument if possible
-- if anything didn't exist, exit failure
main = do
        paths <- getArgs >>= mapM which
        mapM_ putStrLn $ catMaybes paths

        if all isJust paths
          then exitSuccess
          else exitFailure
