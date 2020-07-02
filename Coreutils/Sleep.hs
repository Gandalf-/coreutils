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
import           Data.Maybe         (mapMaybe)
import           System.Exit        (die)
import           Text.Read          (readMaybe)

import           Coreutils.Util

data Sleep = Sleep

instance Util Sleep where
    run _ args
            | null values = die "sleep: [number](s|m|d|w)"
            | otherwise   = mapM_ sleep values
        where
            sleep :: Double -> IO ()
            sleep time = threadDelay . round $ time * 1000000

            values :: [Double]
            values = mapMaybe parse args

parse :: String -> Maybe Double
-- ^ take strings in the format <number>(s|m|d) and try to convert them
-- into a double
parse [] = Nothing
parse ('.':xs) = parse $ "0." <> xs
parse value
        | isAlpha suffix = (*) <$> adjust suffix <*> parse (init value)
        | otherwise      = readMaybe value
    where
        suffix = last value

        adjust :: Char -> Maybe Double
        adjust 's' = Just 1
        adjust 'm' = Just 60
        adjust 'd' = Just $ 60 * 60 * 24
        adjust 'w' = Just $ 60 * 60 * 24 * 7
        adjust _   = Nothing
