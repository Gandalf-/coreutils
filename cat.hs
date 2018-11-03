module Main where

-- cat
--
-- read files from the command line or echo stdin

import           Data.Either        (isRight)
import           System.Directory   (doesFileExist)
import           System.Environment (getArgs)
import           System.Exit


type ErrorOrFile = Either FilePath String


main :: IO ()
main = getArgs >>= mapM collect >>= cat


collect :: FilePath -> IO ErrorOrFile
-- ^ if the file exists grab it's contents, otherwise just keep the name
collect file = do
    exists <- doesFileExist file
    if exists
      then Right <$> readFile file
      else return $ Left file


cat :: [ErrorOrFile] -> IO ()
-- ^ if there are any files display them, otherwise read from stdin
cat [] = getContents >>= putStr
cat xs = do
      mapM_ display xs

      if all isRight xs
        then exitSuccess
        else exitFailure


display :: ErrorOrFile -> IO ()
-- ^ print the contents, or the error
display (Right content) =
    putStr content

display (Left file)  =
    putStrLn $ "cat: " ++ file ++ ": No such file or directory"
