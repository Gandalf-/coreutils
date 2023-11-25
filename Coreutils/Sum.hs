{-# LANGUAGE BangPatterns #-}
module Coreutils.Sum where

import           Coreutils.Util
import           Data.Bits
import           Data.Word
import           Streaming            hiding (Sum)
import qualified Streaming.ByteString as Q
import           System.IO

-- https://en.wikipedia.org/wiki/BSD_checksum

data Sum = Sum

instance Util Sum where
    run _ = runSum

-- | IO

runSum :: [String] -> IO ()
runSum []    = runSum ["-"]
runSum ["-"] = execute "" Q.stdin
runSum [f]   = fExecute f
runSum fs    = mapM_ fExecute fs

fExecute :: FilePath -> IO ()
fExecute f = withFile f ReadMode $ execute f . Q.fromHandle

execute :: FilePath -> Q.ByteStream IO () -> IO ()
execute f s = bsdSum s >>= (putStrLn . uncurry (display f))

-- | Implementation

type CheckSum = Word16
type Blocks   = Integer

bsdSum :: MonadIO m => Q.ByteStream m () -> m (CheckSum, Blocks)
bsdSum stream =
        total <$> Q.fold_ go (0, 0) id stream
    where
        go (!s, !t) !v = (checksum s v, t + 1)
        total (s, t)   = (s, toBlocks t)

toBlocks :: Integer -> Blocks
toBlocks i = ceiling $ (fromIntegral i :: Double) / 1024

checksum :: CheckSum -> Word8 -> CheckSum
checksum !p !v = c3
    where
        !c1 = (p `shiftR` 1) + ((p .&. 1) `shiftL` 15)
        !c2 = c1 + fromIntegral v
        !c3 = c2 .&. 0xffff

display :: FilePath -> CheckSum -> Blocks -> String
display f c b =
        unwords $ filter (not . null) [zeros <> value, spaces <> blocks, f]
    where
        (value, blocks) = (show c, show b)
        zeros  = replicate (5 - length value)  '0'
        spaces = replicate (5 - length blocks) ' '
