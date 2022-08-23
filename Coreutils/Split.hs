{-# LANGUAGE OverloadedStrings #-}

module Coreutils.Split where

-- split
--
-- break a file into chunks by line count, byte count, or figure out the size based on a
-- goal

import           Control.Monad
import           Coreutils.Util
import qualified Data.ByteString.Lazy  as L
import           Data.Char
import           Data.Int
import           Data.List
import           System.Console.GetOpt
import           System.Exit
import           System.IO
import           Text.Read

data Split = Split

instance Util Split where
    run _ = splitMain

data Runtime =
    -- what kinds of splits are we doing?
          RunBytes Int
        | RunLines Int
        | RunChunk Int
    deriving (Show, Eq)

data File = File Handle (Maybe Int)

data Options = Options
        { optSuffixLength :: Int
        , optExtraSuffix  :: String
        , optNumeric      :: Bool
        , optRuntime      :: Runtime
        }

splitMain :: [String] -> IO ()
-- parse arguments and check for obvious errors
splitMain args = do
        let (actions, files, errors) = getOpt RequireOrder options args

        unless (null errors) $ do
            mapM_ putStr errors
            exitFailure

        (prefix, file) <- case files of
            []           -> return ("x", File stdin Nothing)
            [fn]         -> (,) "x" <$> getHandleAndSize fn
            [fn, prefix] -> (,) prefix <$> getHandleAndSize fn
            _            -> die "split cannot operate on more than one file at a time"

        either die (\opts -> runSplit opts file prefix)
            $ foldM (flip id) defaults actions
    where
        getHandleAndSize :: FilePath -> IO File
        -- take the filename, acquire a handle and determine it's size
        getHandleAndSize "-" = pure $ File stdin Nothing
        getHandleAndSize filename = do
            handle <- openBinaryFile filename ReadMode
            seekable <- hIsSeekable handle
            size <- if seekable
                then Just . fromIntegral <$> hFileSize handle
                else pure Nothing
            pure $ File handle size

runSplit :: Options -> File -> String -> IO ()
-- switchboard
runSplit (Options s e n r) file prefix =
        case r of
            RunBytes v -> splitBytes file v filenames
            RunLines v -> splitLines file v filenames
            RunChunk v -> splitChunk file v filenames
    where
        filenames = filenameGenerator prefix n s e

splitBytes :: File -> Int -> [FilePath] -> IO ()
-- simplest split, just by byte ranges
splitBytes _ _ [] = outOfFilenames
splitBytes f@(File h _) n (fn:fs) = do
        L.hGet h n >>= L.writeFile fn
        hIsEOF h   >>= flip unless (splitBytes f n fs)

splitLines :: File -> Int -> [FilePath] -> IO ()
-- create a stream of lines, group them, write them out
splitLines (File h _) n paths =
        L.hGetContents h >>= go paths . L.split newline
    where
        go :: [FilePath] -> [L.ByteString] -> IO ()
        go [] _ = outOfFilenames
        go _ [] = pure ()
        go (fn:fs) bs = do
            let elements = take n bs
            L.writeFile fn $
                if length elements == n
                    -- we're somewhere in the middle of the file
                    then L.snoc (L.intercalate "\n" elements) newline

                    -- don't tack an extra newline on the end of the file
                    else L.intercalate "\n" elements
            go fs (drop n bs)

        newline = 10

splitChunk :: File -> Int -> [FilePath] -> IO ()
splitChunk (File h Nothing) n paths = do
-- size is unknown, have to read it all to get the length
        bs <- L.hGetContents h
        let chunks    = fromIntegral n
            fileSize  = L.length bs
            chunkSize = fileSize `div` chunks
        chunkWriter chunks paths chunkSize bs

splitChunk (File h (Just s)) n paths = do
-- we can skip reading everything into memory since we already know the size
        bs <- L.hGetContents h
        chunkWriter chunks paths chunkSize bs
    where
        chunks    = fromIntegral n
        chunkSize = fileSize `div` chunks
        fileSize  = fromIntegral s

chunkWriter :: Int64 -> [FilePath] -> Int64 -> L.ByteString -> IO ()
chunkWriter _ [] _ _ = outOfFilenames
chunkWriter i (fn:fs) chunkSize bs
        -- last iteration, it gets everything remaining
        | i == 1    = L.writeFile fn bs

        -- other iteration, write our chunk and continue
        | otherwise = do
            L.writeFile fn $ L.take chunkSize bs
            chunkWriter (i - 1) fs chunkSize $ L.drop chunkSize bs

-- | helpers
outOfFilenames :: IO ()
outOfFilenames = die "split could not generate any more output filenames"

adjustment :: String -> Maybe Int
-- convert 10K  to 10240 (10 * 1024), etc
-- convert 10KB to 10000 (10 * 1000), etc
adjustment xs = case span isNumber characters of
        ("", _)       -> Nothing
        (ys, "")      -> Just $ read ys
        (ys, [v,'b']) -> (* read ys) <$> adjust v 1000
        (ys, [v])     -> (* read ys) <$> adjust v 1024
        _             -> Nothing
    where
        characters = map toLower xs

adjust :: Char -> Int -> Maybe Int
adjust n value = (\x -> value ^ (x + 1)) <$> elemIndex n "kmgtpezy"

filenameGenerator :: String -> Bool -> Int -> String -> [String]
-- ^ lazy list of filenames that conform to these options. if more filenames are
-- required than can be generated by the arguments provided, this calls itself with the
-- width increased by 1, making an infinite list
--
-- 9 and z aren't included in the 'main' body of results since they're needed to prefix
-- the next group so we maintain ordering
filenameGenerator prefix numeric width suffix =
        initial <> more
    where
        initial =
            map (\i -> prefix <> i <> suffix)
            $ filter (\i -> head i /= next)
            $ replicateM width characters
        more = filenameGenerator (prefix <> [next]) numeric (width + 1) suffix

        characters
            | numeric   = concatMap show ([0..9] :: [Integer])
            | otherwise = ['a'..'z']

        next = last characters

-- | options parsing

defaults :: Options
defaults = Options
        { optSuffixLength = 2
        , optExtraSuffix  = ""
        , optNumeric      = False
        , optRuntime      = RunLines 1000
        }

options :: [OptDescr (Options -> Either String Options)]
options =
    [ Option "a" ["suffix-length"]
        (ReqArg
            (\arg opt -> case readMaybe arg of
                Nothing  -> Left $ "could not parse " <> arg <> " as a number"
                (Just n) -> Right opt { optSuffixLength = n })
            "N")
        "generate suffixes of length N (default 2)"

    , Option "" ["additional-suffix"]
        (ReqArg
            (\arg opt -> Right opt { optExtraSuffix = arg })
            "SUFFIX")
        "append an additional suffix to file names"

    , Option "d" []
        (NoArg
            (\opt -> Right opt { optNumeric = True}))
        "use numeric suffixes starting at 0, not alphabetic"

    , Option "b" ["bytes"]
        (ReqArg
            (\arg opt -> case adjustment arg of
                Nothing  -> Left $ "could not parse " <> arg <> " as a size"
                (Just n) -> Right opt { optRuntime = RunBytes n})
            "SIZE")
        "put SIZE bytes per output file"

    , Option "l" ["lines"]
        (ReqArg
            (\arg opt -> case readMaybe arg of
                Nothing  -> Left $ "could not parse " <> arg <> " as a number"
                (Just n) -> Right opt { optRuntime = RunLines n})
            "NUMBER")
        "put NUMBER lines/records per output file"

    , Option "n" ["number"]
        (ReqArg
            (\arg opt -> case readMaybe arg of
                Nothing  -> Left $ "could not parse " <> arg <> " as a number"
                (Just n) -> Right opt { optRuntime = RunChunk n})
            "CHUNKS")
        "generate CHUNKS output files"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "split" options))
        "Show this help text"
    ]
