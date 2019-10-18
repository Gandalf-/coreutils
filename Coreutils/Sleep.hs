module Coreutils.Sleep where

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
import           System.Exit        (die)
import           Text.Read          (readMaybe)

import           Coreutils.Util

data Sleep = Sleep

instance Util Sleep where
    run _ args =
            if all isJust values
                then mapM_ sleep $ catMaybes values
                else die "sleep: [number](s|m|d)"
        where
            sleep :: Int -> IO ()
            sleep time = threadDelay $ time * 1000000

            values = map parse args


parse :: String -> Maybe Int
-- ^ take strings in the format <number>(s|m|d) and try to convert them
-- into a Int
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
