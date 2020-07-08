{-# LANGUAGE OverloadedStrings #-}

module Coreutils.Yes where

import qualified Data.ByteString.Char8     as L
import qualified Data.ByteString.Streaming as Q

import           Coreutils.Util

-- yes
--
-- repeat the command line arguments to stdout, quickly!

data Yes = Yes

instance Util Yes where
    run _ args = yes $ Q.fromStrict $ L.pack $ align $ concat args

yes :: Q.ByteString IO () -> IO ()
yes = Q.stdout . Q.cycle

align :: String -> String
-- pad the string to be page aligned. difference between 19MB/s and 1.4GB/s
align xs
    | null xs           = take block $ cycle "y\n"
    | length xs > block = xs
    | otherwise         = take n $ cycle xs <> "\n"
    where
        n = (block `div` length xs) * length xs
        block = 4096
