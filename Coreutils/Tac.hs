module Coreutils.Tac where

-- tac
--
-- read files from the command line or echo stdin and print them with lines reversed

import qualified Data.ByteString.Char8           as C
import qualified Data.ByteString.Streaming.Char8 as Q
import           Streaming
import qualified Streaming.Prelude               as S
import           System.IO

import           Coreutils.Util

data Tac = Tac

instance Util Tac where
    run _ = tacMain


tacMain :: [String] -> IO ()
tacMain args
        | null args = switch "-"
        | otherwise = mapM_ switch args
    where
        switch "-"  = stdinTac
        switch path = liftIO (withFile path ReadMode fileTac)

stdinTac :: IO ()
stdinTac = unlines . reverse . lines <$> getContents >>= putStr

fileTac :: MonadIO m => Handle -> m ()
fileTac = Q.stdout
        . Q.unlines            -- Q.ByteString m ()
        . S.subst Q.chunk      -- Stream (Q.ByteString m) m ()
        . S.dropWhile C.null   -- final newline shows up as empty element
        . S.map C.reverse      -- back to readable lines
        . mapped Q.toStrict    -- Stream (Of C.ByteString) m ()
        . Q.lines              -- Stream (Q.ByteString m) m ()
        . readBackwards        -- Q.ByteString m ()

readBackwards :: MonadIO m => Handle -> Q.ByteString m ()
-- read the file backwards by characters, the handle must be seek-able
readBackwards h = do
        size <- liftIO (hFileSize h)
        mapM_ (\(l, a) -> reader l a h) $ locations size block
    where
        block = 1024 :: Integer

reader :: MonadIO m => Integer -> Int -> Handle -> Q.ByteString m ()
-- seek to the location, read amount bytes into a streaming bytestring
reader location amount h = do
        liftIO (hSeek h AbsoluteSeek location)
        liftIO (C.reverse <$> C.hGetSome h amount) >>= Q.chunk

locations :: Integer -> Integer -> [(Integer, Int)]
-- generate the range of seeks + amounts that we'll use with reader
locations size block
        | size < block = [(0, amount)]
        | otherwise    = most <> [(0, rest)]
    where
        most = zip [size - block, size - block - block..0] (repeat amount)
        rest = fromIntegral $ size `mod` block

        amount = fromIntegral block
