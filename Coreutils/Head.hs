{-# LANGUAGE OverloadedStrings #-}

module Coreutils.Head where

-- head
--
-- show some number of lines or characters from the top or bottom of files
-- or stdin. if both -c and -l are provided, -c is preferred
--
-- lazy implementation allows things like `head -l 5 /dev/urandom`
-- negative options `-l -10`, `-c -100` require the entire file to fit in memory


import           Control.Monad
import qualified Data.ByteString.Lazy  as L
import qualified Data.ByteString.Lazy.Char8  as C
import           GHC.Int               (Int64)
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
        , _filesize :: Maybe Int
        }

runHead :: Options -> [File] -> IO ()
runHead (Options quiet runtime) fs =
        mapM_ (wrapper action) fs
    where
        wrapper
            | not quiet && length fs > 1 = verbose
            | otherwise                  = id

        action :: File -> IO ()
        action = case runtime of
            RunBytes v -> headBytes v
            RunLines v -> headLines v

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
        getFile "-" = pure $ File stdin "-" Nothing
        getFile fn = do
                h <- openBinaryFile fn ReadMode
                size <- fromIntegral <$> hFileSize h
                pure $ File h fn (Just size)

-- | Helpers

headBytes :: Int64 -> File -> IO ()
headBytes n (File h _ size)
        | n > 0     = L.hGet h amount >>= L.putStr
        | n == 0    = pure ()
        | otherwise = case size of
            -- it's negative and we know the total file size
            (Just s) -> L.hGet h (s + amount) >>= L.putStr

            -- it's negative and we have to read in everything to know the size
            Nothing  -> do
                -- TODO something like negative lines
                bytes <- L.hGetContents h
                L.putStr $ L.take (L.length bytes + n) bytes
    where
        amount = fromIntegral n

headLines :: Int64 -> File -> IO ()
headLines n (File h _ _)
        | n > 0     = take amount <$> lazyLines >>= out
        | n == 0    = pure ()
        | otherwise = lazyLines >>= out . clever []
    where
        lazyLines :: IO [C.ByteString]
        lazyLines = C.lines <$> C.hGetContents h

        out :: [L.ByteString] -> IO ()
        out = C.putStrLn . C.intercalate "\n"

        clever :: [C.ByteString] -> [C.ByteString] -> [C.ByteString]
        clever previous ls = do
            let new = take amount ls
            if length new < amount
                then take (length new + amount - amount) $ previous <> new
                else previous <> clever new (drop amount ls)

        amount = fromIntegral $ abs n


verbose :: (File -> IO ()) -> File -> IO ()
-- ^ used when multiples files are processed together
verbose fn f = do
        putStrLn $ "==> " <> _filename f <> " <=="
        fn f
        putStrLn "" -- TODO don't when this is the last file

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
