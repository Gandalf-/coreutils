{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Coreutils.Head where

-- head
--
-- show some number of lines or characters from the top or bottom of files
-- or stdin.
--
-- this has constant memory usage even with negative values on streams

import           Control.Monad
import qualified Data.ByteString.Lazy            as L
import qualified Data.ByteString.Lazy.Char8      as BC
import qualified Data.ByteString.Streaming       as Q
import qualified Data.ByteString.Streaming.Char8 as C
import           GHC.Int                         (Int64)
import qualified Streaming.Prelude               as S
import           System.Console.GetOpt
import           System.Exit
import           System.IO

import           Coreutils.Util

data Head = Head

instance Util Head where
    run _ = headMain

data Runtime = RunBytes Int64 | RunLines Int64

data Options = Options
        { optQuiet   :: Bool
        , optRuntime :: Runtime
        }

data File = File
        { _handle   :: Handle
        , _filename :: FilePath
        , _filesize :: Maybe Int64
        }

headMain :: [String] -> IO ()
headMain args = do
        let (actions, filenames, errors) = getOpt RequireOrder options args

        unless (null errors) $ do
            mapM_ putStr errors
            exitFailure

        files <- case filenames of
            [] -> (: []) <$> getFile "-"
            fs -> mapM getFile fs

        case foldM (flip id) defaults actions of
            Left   err -> die err
            Right opts -> runHead opts files
    where
        getFile :: FilePath -> IO File
        -- take the filename, acquire a handle and determine it's size
        getFile "-" = pure $ File stdin "-" Nothing
        getFile name = do
            h <- openBinaryFile name ReadMode
            size <- hIsSeekable h >>= \case
                True  -> Just . fromIntegral <$> hFileSize h
                False -> pure Nothing
            pure $ File h name size

runHead :: Options -> [File] -> IO ()
-- switchboard
runHead (Options quiet runtime) fs =
        go wrapper fs
    where
        action :: File -> IO ()
        action = case runtime of
            RunBytes v -> headBytes v
            RunLines v -> headLines v

        -- runner that tells its callback when it's being passed the last element
        go _ []     = pure ()
        go f [x]    = f True  action x
        go f (x:xs) = f False action x >> go f xs

        wrapper :: Bool -> (File -> IO ()) -> File -> IO ()
        wrapper
            | not quiet && multiple = verbose
            | otherwise             = const id

        -- don't use length because there could be hundreds of files, and we don't want
        -- to keep the handles open for this list
        multiple = length (take 2 fs) == 2

verbose :: Bool -> (File -> IO ()) -> File -> IO ()
-- ^ used when multiples files are processed together
verbose final fn f = do
        putStrLn $ "==> " <> _filename f <> " <=="
        fn f
        unless final $ putStrLn ""

-- | Helpers

headBytes :: Int64 -> File -> IO ()
-- take some number of bytes from the beginning of the file, or ommit some number from
-- the end
headBytes n (File h _ s)
        | n > 0     = Q.stdout . Q.hGet h $ fromIntegral n
        | n == 0    = pure ()
        | otherwise = case s of
            (Just size) ->
                when (size - amount > 0) $
                    Q.hPut stdout . Q.hGet h . fromIntegral $ size - amount

            -- it's negative and we have don't have the size
            Nothing  -> clever Q.empty $ Q.hGetContents h
    where
        -- read the stream 'amount' at a time; if we get less than expected we know
        -- we're at the end
        clever previous ls = do
            let new = Q.take amount ls
            len <- Q.length_ new

            if len < fromIntegral amount
                then do
                    prev_len <- Q.length_ previous
                    let size = fromIntegral $ len + prev_len - fromIntegral amount
                    Q.hPut stdout $ Q.take size $ previous <> new
                else do
                    Q.hPut stdout previous
                    clever new (Q.drop amount ls)

        amount = abs n

headLines :: Int64 -> File -> IO ()
-- take some number of lines from the beginning of the file, or ommit some number from
-- the end
headLines n (File h _ _)
        | n > 0     = C.putStr $ C.unlines $ S.take amount $ C.lines $ C.hGetContents h
        | n == 0    = pure ()
        | otherwise = lazyLines >>= out . chunkedStreamRead []
    where
        lazyLines :: IO [BC.ByteString]
        lazyLines = BC.lines <$> BC.hGetContents h

        out :: [L.ByteString] -> IO ()
        out [] = pure ()
        out xs = BC.putStrLn $ BC.intercalate "\n" xs

        chunkedStreamRead :: [BC.ByteString] -> [BC.ByteString] -> [BC.ByteString]
        -- read the stream 'amount' at a time; if we get less than expected we know
        -- we're at the end
        chunkedStreamRead previous ls = do
            let new = take amount ls
                len = length new
            if len < amount
                then take (len + length previous - amount) $ previous <> new
                else previous <> chunkedStreamRead new (drop amount ls)

        amount = fromIntegral $ abs n

        -- chunkedStreamRead :: [C.ByteString] -> [C.ByteString] -> [C.ByteString]
        -- read the stream 'amount' at a time; if we get less than expected we know
        -- we're at the end
        {-
        clever :: Monad m => C.ByteString m r -> C.ByteString m r -> S.Stream (S.Of (C.ByteString m r)) m ()
        clever previous ls = do
            let new = Q.take (fromIntegral amount) ls
                len = 234
            -- len <- Q.length_ new
            if len < amount
                then do
                    prev_len <- Q.length_ previous
                    S.yield $ Q.take (fromIntegral $ len + prev_len - fromIntegral amount) $ previous <> new
                else undefined -- previous <> clever new (drop amount ls)
        -}

{-
g :: S.Stream (Of Integer) IO ()
g = do
    S.each [1..4]
    S.each [5..6]
    S.each [7..9]

h s = do
    let a = S.take 5 s
        b = S.drop 5 s
    b
    a

type ByteStream = S.Stream (C.ByteString IO) IO ()

-- f :: S.Stream (Of a) IO () -> Int -> S.Stream (Of a) IO () -> S.Stream (Of a) IO ()
f :: ByteStream -> Int -> ByteStream -> ByteStream
f prev n s = do
    let this = S.take n s
        more = S.drop n s

    let t = S.length_ $ S.copy this
    tl <- t
    -- let tl = S.effects $ S.length $ S.copy this

    if tl < n
        then do
            let p = S.length_ $ S.copy prev
            pl <- p
            S.take (tl + pl - n) $ do
                p
                t
        else do
            prev
            f this n more
-}

-- | Options

defaults :: Options
defaults = Options
        { optQuiet = False
        , optRuntime = RunLines 10
        }

options :: [OptDescr (Options -> Either String Options)]
options =
    [ Option "n" ["lines"]
        (ReqArg
            (\arg opt -> case reads arg of
              [(n, "")] -> Right opt { optRuntime = RunLines n }
              _         -> Left $ "error: '" <> arg <> "' is not a number")
            "LINES")
        "Number of lines"

    , Option "c" ["bytes"]
        (ReqArg
            (\arg opt -> case reads arg of
              [(n, "")] -> Right opt { optRuntime = RunBytes n }
              _         -> Left $ "error: '" <> arg <> "' is not a number")
            "LINES")
        "Number of characters"

    , Option "q" ["quiet", "silent"]
        (NoArg
            (\opt -> Right opt { optQuiet = True }))
        "Do not show headers for files"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "head" options))
        "Show this help text"
    ]
