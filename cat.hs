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


main :: IO ()
-- ^ attempt to use all arguments as files
main = getArgs >>= mapM (try . readFile) >>= cat


cat :: [Either IOException String] -> IO ()
cat [] = getContents >>= putStr

cat files = do
        mapM_ display files
        when (any isLeft files) exitFailure
    where
        display (Left exception) = print exception
        display (Right content)  = putStr content
