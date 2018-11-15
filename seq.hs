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
handle (start : increment : end : _) =
      mapM_ print [start, start + increment .. end]

handle (start : end : _) = handle [start, 1, end]
handle [end]             = handle [1, 1, end]
handle _                 = help


help :: IO ()
help = do
      putStrLn "usage: seq [end]"
      putStrLn "           [start] [end]"
      putStrLn "           [start] [increment] [end]"
