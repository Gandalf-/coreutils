module Main where

-- cat
--
-- read files from the command line or echo stdin
-- soldiers on when some files do not exist, but reports failure at the end

import           Control.Exception  (IOException, try)
import           Control.Monad      (when)
import           Data.Either        (isLeft)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import           System.IO          (hPutStrLn, stderr)

type Argument    = String
type FileContent = String

collect :: [Argument] -> IO [Either IOException FileContent]
collect = mapM (try . readFile)

display :: [Either IOException FileContent] -> IO ()
display [] = getContents >>= putStr

display files = do
        mapM_ toConsole files
        when (any isLeft files) exitFailure
    where
        toConsole (Left exception) = hPutStrLn stderr $ show exception
        toConsole (Right content)  = putStr content

main :: IO ()
main = getArgs >>= collect >>= display
