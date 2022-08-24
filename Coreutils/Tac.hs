module Coreutils.Tac where

-- tac
--
-- read files from the command line or echo stdin and print them with lines reversed

import qualified Data.ByteString.Char8      as C
import           Streaming
import qualified Streaming.ByteString.Char8 as Q
import qualified Streaming.Prelude          as S
import           System.IO

import           Coreutils.Util

data Tac = Tac

instance Util Tac where
    run _ = tacMain

type SeekPosition = Integer

tacMain :: [String] -> IO ()
tacMain args
        | null args = switch "-"
        | otherwise = mapM_ switch args
    where
        switch "-"  = stdinTac
        switch path = liftIO (withFile path ReadMode (Q.stdout . fileTac))

stdinTac :: IO ()
stdinTac = C.getContents >>= C.putStr . C.unlines . reverse . C.lines

fileTac :: MonadIO m => Handle -> Q.ByteStream m ()
fileTac = Q.unlines            -- Q.ByteString m ()
        . S.subst Q.chunk      -- Stream (Q.ByteString m) m ()
        . S.map C.reverse      -- back to readable lines
        . mapped Q.toStrict    -- Stream (Of C.ByteString) m ()
        . Q.lines              -- Stream (Q.ByteString m) m ()
        . readBackwards        -- Q.ByteString m ()

readBackwards :: MonadIO m => Handle -> Q.ByteStream m ()
-- read the file backwards by characters, the handle must be seek-able
readBackwards h = do
        size <- liftIO (hFileSize h)
        mapM_ (uncurry $ seeker h) $ locations size block
    where
        block = 1024 * 32 :: Integer

seeker :: MonadIO m => Handle -> SeekPosition -> Int -> Q.ByteStream m ()
-- seek to the location, read 'amount' bytes into a streaming bytestring
seeker h location amount = do
        liftIO (hSeek h AbsoluteSeek location)
        liftIO (C.reverse <$> C.hGetSome h amount) >>= Q.chunk

locations :: Integer -> Integer -> [(SeekPosition, Int)]
-- generate the range of seeks + amounts that we'll use with seeker. these work
-- backwards in steps of 'block' size, with a final smaller block if needed
locations size block
        | size < block = [(0, fromIntegral size)]
        | rest > 0     = most <> [(0, rest)]
        | otherwise    = most
    where
        most = zip [size - block, size - block - block..0] (repeat amount)
        rest = fromIntegral $ size `mod` block

        amount = fromIntegral block
