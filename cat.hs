module Main where

-- cat
--
-- read files from the command line or echo stdin

import           Control.Exception  (IOException, try)
import           Data.Either        (isRight)
import           System.Environment (getArgs)
import           System.Exit


type ErrorOrFile = Either IOException String


main :: IO ()
main = getArgs >>= mapM (try . readFile) >>= cat


cat :: [ErrorOrFile] -> IO ()
cat [] = getContents >>= putStr
cat xs = do
      mapM_ display xs

      if all isRight xs
        then exitSuccess
        else exitFailure
    where
        display (Right content) = putStr content
        display (Left e)        = print e
