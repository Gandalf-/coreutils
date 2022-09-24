module Coreutils.Sponge  where

import           Control.Monad
import qualified Data.ByteString as B

import           Coreutils.Util

data Sponge = Sponge

instance Util Sponge where
    run _ args = case args of
        []         -> sponge B.putStr
        ["-"]      -> sponge B.putStr
        ["-h"]     -> putStrLn usage
        ["--help"] -> putStrLn usage
        xs         -> sponge (forM_ xs . flip B.writeFile)

sponge :: (B.ByteString -> IO ()) -> IO ()
sponge writer = B.getContents >>= writer

usage :: String
usage = "sponge: [file...]\n\tSoak up stdin and write to file(s) or stdout"
