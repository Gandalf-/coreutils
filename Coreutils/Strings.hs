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
        filter valid . B.splitWith (not . printable)
    where
        valid bs = B.length bs >= fromIntegral len

printable :: Char -> Bool
-- FreeBSD contrib/elftoolchain/strings/strings.c
printable c
        | v < 0     = False
        | v > 255   = False
        | c == '\t' = True
        | otherwise = isPrint c
    where
        v = ord c

-- | Options

newtype Options = Options { optLength :: Int }
    deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options 4

parseArgs :: [String] -> Either String (Options, [FilePath])
parseArgs = parser defaultOptions

parser :: Options -> [String] -> Either String (Options, [String])
parser o [] = Right (o, [])
parser o ("--":xs) =
    Right (o, xs)
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
