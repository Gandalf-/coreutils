module Main where

-- echo
--
-- display something to the console
-- supports -n

import           System.Environment (getArgs)


echo :: String -> IO ()
echo ('-' : 'n' : ' ' : xs) = putStr xs
echo xs                     = putStrLn xs


main :: IO ()
main = getArgs >>= echo . unwords
