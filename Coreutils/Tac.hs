module Coreutils.Tac where

-- tac
--
-- read files from the command line or echo stdin and print them with lines reversed

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8           as C
import qualified Data.ByteString.Lazy.Char8      as L
import qualified Data.ByteString.Streaming.Char8 as Q
import           Streaming
import qualified Streaming.Prelude               as S
import           System.Exit
import           System.Info
import           System.IO

import           Coreutils.Util

data Tac = Tac

instance Util Tac where
    run _ = undefined


-- fStream :: Handle ->
fstream h = do
        size <- hFileSize h
        let ranges = [size - iBlock,size - iBlock - iBlock..iBlock]
        L.concat <$> mapM (go h block) ranges
    where
        block = 1024 * 32
        iBlock = fromIntegral block

go h l s = do
        print s
        hSeek h AbsoluteSeek s
        L.fromStrict . C.reverse <$> C.hGetSome h l
