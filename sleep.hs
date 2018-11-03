module Main where

-- sleep
--
-- delay for some amount of time
-- if multiple arguments are given, sleep for the total
-- supports the following suffixes
--      s -> seconds
--      m -> minutes
--      d -> days

import           Control.Concurrent (threadDelay)
import           Data.Char          (isAlpha)
import           Data.Maybe         (catMaybes, isJust)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import           Text.Read          (readMaybe)


parse :: String -> Maybe Int
parse [] = Nothing
parse value
        | isAlpha suffix = (*) <$> adjust suffix <*> parse (init value)
        | otherwise      = readMaybe value
    where
        suffix = last value

        adjust :: Char -> Maybe Int
        adjust 's' = Just 1
        adjust 'm' = Just 60
        adjust 'd' = Just $ 60 * 24
        adjust _   = Nothing


main :: IO ()
main = do
        values <- map parse <$> getArgs

        if all isJust values
            then mapM_ sleep $ catMaybes values
            else do
                putStrLn "sleep: [number](s|m|d)"
                exitFailure
    where
        sleep :: Int -> IO ()
        sleep time = threadDelay $ time * 1000000
