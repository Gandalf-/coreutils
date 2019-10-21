module Coreutils.Seq where

-- seq
--
-- produce a sequence of numbers

import           Data.Maybe     (catMaybes, isJust)
import           System.Exit    (die)
import           Text.Read      (readMaybe)

import           Coreutils.Util

data Seq = Seq

instance Util Seq where
    run _ args =
            if all isJust arguments
                then case runSeq $ catMaybes arguments of
                    (Just s) -> mapM_ print s
                    Nothing  -> die help

                else die "seq: unable to parse arguments as numbers"
        where
            arguments = map readMaybe args


runSeq :: [Integer] -> Maybe [Integer]
-- ^ produce a list of numbers, use pattern matching to represent the
-- default values and error handling
runSeq [start, increment, end] = Just [start, start + increment .. end]
runSeq [start, end]            = runSeq [start, 1, end]
runSeq [end]                   = runSeq [1, 1, end]
runSeq _                       = Nothing


help :: String
help = unlines [
      "usage: seq [end]"
    , "           [start] [end]"
    , "           [start] [increment] [end]"
    ]
