module Coreutils.Sponge  where

import           Control.Monad
import qualified Data.ByteString as B

import           Coreutils.Util

data Sponge = Sponge

instance Util Sponge where
    run _ = spongeMain

spongeMain :: [String] -> IO ()
spongeMain []         = sponge B.putStr
spongeMain ["-"]      = sponge B.putStr

spongeMain ["-h"]     = putStrLn usage
spongeMain ["--help"] = putStrLn usage

spongeMain xs         = sponge (forM_ xs . flip B.writeFile)

sponge :: (B.ByteString -> IO ()) -> IO ()
sponge writer = B.getContents >>= writer

usage :: String
usage = "sponge: [file...]\n\tSoak up stdin and write to file(s) or stdout"
