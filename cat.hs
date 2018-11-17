module Main where

-- cat
--
-- read files from the command line or echo stdin

import           Control.Exception  (IOException, try)
import           Control.Monad      (when)
import           Data.Either        (isLeft)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

type ErrorOrFile = Either IOException String
type ErrorsOrFiles = [ErrorOrFile]

main :: IO ()
main = getArgs >>= mapM (try . readFile) >>= cat

cat :: ErrorsOrFiles -> IO ()
cat [] = getContents >>= putStr
cat xs = do
        mapM_ display xs
        when (any isLeft xs) exitFailure
    where
        display (Left exception) = print exception
        display (Right content)  = putStr content
