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

tacMain :: [String] -> IO ()
tacMain args
        | null args = switch "-"
        | otherwise = mapM_ switch args
    where
        switch "-"  = stdinTac
        switch path = liftIO (withFile path ReadMode (Q.stdout . fileTac))

reverseLines :: C.ByteString -> C.ByteString
reverseLines bs
        | C.null bs          = bs
        | C.last bs == '\n'  = C.unlines (reverse (C.lines bs))
        | otherwise          = C.intercalate (C.singleton '\n') (reverse (C.lines bs))

stdinTac :: IO ()
stdinTac = C.getContents >>= C.putStr . reverseLines

endsWithLF :: Handle -> Integer -> IO Bool
endsWithLF _ 0    = pure False
endsWithLF h size = do
        hSeek h AbsoluteSeek (size - 1)
        (== C.singleton '\n') <$> C.hGet h 1

fileTac :: MonadIO m => Handle -> Q.ByteStream m ()
fileTac h = do
        size <- liftIO (hFileSize h)
        if size == 0 then pure ()
        else do
            endsLF <- liftIO (endsWithLF h size)
            let readSize = if endsLF then size - 1 else size
                combine  = if endsLF then Q.unlines
                           else Q.intercalate (Q.chunk (C.singleton '\n'))
            combine . inner . Q.split '\n' $ readBackwards h readSize
    where
        inner = S.subst Q.chunk . S.map C.reverse . mapped Q.toStrict

readBackwards :: MonadIO m => Handle -> Integer -> Q.ByteStream m ()
-- read the file backwards by characters, the handle must be seek-able
readBackwards h size =
        mapM_ seek $ locations size block
    where
        block = 1024 * 32 :: Integer
        seek (pos, amount) = do
            liftIO (hSeek h AbsoluteSeek pos)
            liftIO (C.reverse <$> C.hGetSome h amount) >>= Q.chunk

locations :: Integer -> Integer -> [(Integer, Int)]
-- generate the range of seeks + amounts that we'll use to read backwards. these
-- work backwards in steps of 'block' size, with a final smaller block if needed
locations size block
        | size < block = [(0, fromIntegral size)]
        | rest > 0     = most <> [(0, rest)]
        | otherwise    = most
    where
        most = zip [size - block, size - block - block..0] (repeat amount)
        rest = fromIntegral $ size `mod` block

        amount = fromIntegral block
