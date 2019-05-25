module Main where

-- pwd
--
-- output the current working directory

import           System.Directory (getCurrentDirectory)

main :: IO ()
main = getCurrentDirectory >>= putStrLn
