module Main where

-- yes
--
-- repeat the command line arguments to stdout

import           System.Environment (getArgs)


yes :: String -> IO ()
yes [] = putStrLn "y" >> yes []
yes xs = putStrLn xs  >> yes xs

main :: IO ()
main = concat <$> getArgs >>= yes
