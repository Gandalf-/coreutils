module Coreutils.Strings where

import           Control.Monad
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char
import           System.Exit

import           Coreutils.Util

data Strings = Strings

instance Util Strings where
    run _ = stringsMain

-- | IO

stringsMain :: [String] -> IO ()
stringsMain args = do
    (opts, files) <- either die return $ parseArgs args
    let search = mapM_ B.putStrLn . strings (optLength opts)

    if null files
        then B.getContents >>= search
        else mapM_ (B.readFile >=> search) files

-- | Implementation

strings :: Int -> ByteString -> [ByteString]
strings len =
        filter valid . B.splitWith dirt
    where
        valid bs = B.length bs >= fromIntegral len

dirt :: Char -> Bool
-- BSD 4.3
-- https://opensource.apple.com/source/cctools/cctools-822/misc/strings.c.auto.html
dirt '\n' = False
dirt '\f' = False
dirt '\DEL' = True
dirt x
    | v > 0o200 || v < 32 = True
    | otherwise = False
    where
        v = ord x

-- | Options

newtype Options = Options { optLength :: Int }
    deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options 4

parseArgs :: [String] -> Either String (Options, [FilePath])
parseArgs = parser defaultOptions

parser :: Options -> [String] -> Either String (Options, [String])
parser o [] = Right (o, [])
parser o ("-n":n:xs) = do
    v <- getNumber n
    parser (o { optLength = v }) xs
parser o (('-':x):xs) = do
    v <- getNumber x
    parser (o { optLength = v }) xs
parser o (x:xs) = do
    (o', xs') <- parser o xs
    return (o', x:xs')

getNumber :: String -> Either String Int
getNumber s
    | all isDigit s = Right $ read s
    | otherwise     = Left $ s <> " is not a number"
