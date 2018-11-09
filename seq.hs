module Main where

-- seq
--
-- produce a sequence of numbers

import           Data.Maybe         (catMaybes, isJust)
import           System.Environment (getArgs)
import           System.Exit        (die)
import           Text.Read          (readMaybe)


main :: IO ()
main = do
      args <- map readMaybe <$> getArgs

      if all isJust args
        then handle $ catMaybes args
        else die "seq: unable to parse arguments as numbers"


handle :: [Integer] -> IO ()
-- ^ produce a list of numbers, use pattern matching to represent the
-- default values and error handling
handle (first : increment : last : _) =
      mapM_ print [first, first + increment .. last]

handle (first : last : _) = handle [first, 1, last]
handle [last]             = handle [1, 1, last]
handle _                  = help


help :: IO ()
help = do
      putStrLn "usage: seq [last]"
      putStrLn "           [first] [last]"
      putStrLn "           [first] [increment] [last]"
