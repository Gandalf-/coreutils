{-# LANGUAGE OverloadedStrings #-}

module Coreutils.Yes where

import qualified Data.ByteString.Char8     as C
import qualified Data.ByteString.Streaming as Q

import           Coreutils.Util

-- yes
--
-- repeat the command line arguments to stdout, quickly!

data Yes = Yes

instance Util Yes where
    run _ args = yes $ Q.fromStrict $ C.pack $ align $ unwords args

yes :: Q.ByteString IO () -> IO ()
yes = Q.stdout . Q.cycle

align :: String -> String
-- pad the string to be page aligned. difference between 19MB/s and 1.4GB/s
align [] = align "y"
align xs
        | length xs > block = xs <> "\n"
        | otherwise         = take n $ cycle $ xs <> "\n"
    where
        n = (block `div` length xs) * (length xs + 1)

block :: Int
block = 4096
