{-# LANGUAGE OverloadedStrings #-}

module Coreutils.Yes where

import qualified Data.ByteString.Char8     as L
import qualified Data.ByteString.Streaming as Q

import           Coreutils.Util

-- yes
--
-- repeat the command line arguments to stdout

data Yes = Yes

instance Util Yes where
    run _ args = Q.stdout $ Q.cycle $ yes $ concat args

yes :: String -> Q.ByteString m ()
yes [] = "y\n"
yes xs = Q.fromStrict $ L.pack xs
