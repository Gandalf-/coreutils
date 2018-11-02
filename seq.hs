module Main where

-- seq
--
-- produce a sequence of numbers

import           Data.Maybe         (catMaybes, isJust)
import           System.Environment (getArgs)
import           Text.Read          (readMaybe)


handle :: [Integer] -> IO ()
handle (first : increment : last : _) =
      mapM_ print steps
  where steps = [first, first + increment .. last]

handle (first : last : _) = handle [first, 1, last]
handle [last]             = handle [1, 1, last]
handle _                 = help


help :: IO ()
help = do
      putStrLn "usage: seq [last]"
      putStrLn "           [first] [last]"
      putStrLn "           [first] [increment] [last]"


main :: IO ()
main = do
      args <- map readMaybe <$> getArgs
      if all isJust args
        then handle $ catMaybes args
        else putStrLn "seq: unable to parse arguments as numbers"
