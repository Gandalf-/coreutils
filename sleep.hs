module Main where

import           Control.Concurrent (threadDelay)
import           System.Environment (getArgs)
import           Text.Read          (readMaybe)


sleep :: [Maybe Int] -> IO ()
sleep (Just time : _) = threadDelay $ time * 1000000
sleep _               = putStrLn "sleep: [seconds]"


main :: IO ()
main = map readMaybe <$> getArgs >>= sleep
