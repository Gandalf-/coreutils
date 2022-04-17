{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Coreutils.Cmp where

import Control.Monad
import           GHC.Int               (Int64)
import           GHC.IO.Handle
import qualified Data.ByteString as B
import           Text.Parsec
import           System.Console.GetOpt
import           System.IO

import           Coreutils.Util

data Cmp = Cmp

instance Util Cmp where
    run _ = undefined

-- Core

skipBytes :: Integer -> File -> IO ()
skipBytes n (File h _ s) =
    case s of
        Nothing   -> void $ B.hGet h (fromIntegral n)
        (Just fs) -> hSeek h AbsoluteSeek (min (toInteger fs) n)

-- Execute

execute :: Runtime -> IO ()
execute r = do
    _ <- applySkips r
    pure ()

applySkips :: Runtime -> IO Runtime
applySkips r@(Runtime opts f1 f2) = do
    skipBytes s1 f1
    skipBytes s2 f2
    pure r
    where
        (s1, s2) = optIgnore opts

-- Runtime

data Runtime = Runtime
    { _runOptions :: Options
    , _lfile :: File
    , _rfile :: File
    }

getRuntime :: Config -> IO Runtime
getRuntime (Config opts (f1, f2)) = do
    fh1 <- getFile f1
    fh2 <- getFile f2
    pure $ Runtime opts fh1 fh2

data File = File
    { _handle   :: Handle
    , _filename :: FilePath
    , _filesize :: Maybe Int64
    }

getFile :: FilePath -> IO File
-- take the filename, acquire a handle and determine it's size
getFile "-" = pure $ File stdin "-" Nothing
getFile name = do
    h <- openBinaryFile name ReadMode
    size <- hIsSeekable h >>= \case
        True  -> Just . fromIntegral <$> hFileSize h
        False -> pure Nothing
    pure $ File h name size

-- Configuration

data Config = Config
    { _options :: Options
    , _files :: (FilePath, FilePath)
    }
    deriving (Show, Eq)

getConfig :: [String] -> Either String Config
getConfig args = do
    let (actions, extras, errors) = getOpt RequireOrder options args

    unless (null errors) $ Left (unwords errors)
    opts <- foldM (flip id) defaults actions

    case extras of
        [f1, f2, s1, s2] -> do
            skip1 <- readSkip s1
            skip2 <- readSkip s2
            Right $ Config opts { optIgnore = (skip1, skip2) } (f1, f2)

        [f1, f2, s1] -> do
            skip <- readSkip s1
            Right $ Config (updateOneSkip opts skip) (f1, f2)

        [f1, f2]     -> Right $ Config opts (f1, f2)
        [f1]         -> Right $ Config opts (f1, "-")
        _            -> Left "at least one file is required"
    where
        updateOneSkip :: Options -> Integer -> Options
        updateOneSkip o s1 = o { optIgnore = (s1, s2)}
            where
                (_, s2) = optIgnore o

        readSkip :: String -> Either String Integer
        readSkip s = case parseASkip s of
            Nothing -> Left $ s <> " could not be parsed as SKIP"
            (Just v) -> Right v

-- Parsing

suffix :: Parsec String () Integer
suffix =
    foldl (<|>) k
        [ m, g, t, p, e, y
        , kb, mb, gb, tb, pb, eb, yb
        , pure 1 -- default multiplier if there wasn't a suffix
        ]
    where
        k  = string "K"  >> pure (1024 ^ 1)
        m  = string "M"  >> pure (1024 ^ 2)
        g  = string "G"  >> pure (1024 ^ 3)
        t  = string "T"  >> pure (1024 ^ 4)
        p  = string "P"  >> pure (1024 ^ 5)
        -- "sure", skip the first n exabytes in this file
        e  = string "E"  >> pure (1024 ^ 6)
        y  = string "Y"  >> pure (1024 ^ 7)

        kb = string "kB" >> pure (1000 ^ 1)
        mb = string "mB" >> pure (1000 ^ 2)
        gb = string "gB" >> pure (1000 ^ 3)
        tb = string "tB" >> pure (1000 ^ 4)
        pb = string "pB" >> pure (1000 ^ 5)
        eb = string "eB" >> pure (1000 ^ 6)
        yb = string "yB" >> pure (1000 ^ 7)

bytes :: Parsec String () Integer
bytes = do
    b <- read <$> many1 (oneOf ['0'..'9'])
    m <- suffix
    pure (b * m)

parseOneSkip :: Parsec String () (Integer, Integer)
parseOneSkip = do
    s <- bytes
    pure (s, 0)

parseTwoSkips :: Parsec String () (Integer, Integer)
parseTwoSkips = do
    s1 <- bytes
    _ <- char ':'
    s2 <- bytes
    pure (s1, s2)

parseSkip :: String -> Maybe (Integer, Integer)
parseSkip skip =
    case parse (p <* eof) "cmp" skip of
        (Left _)  -> Nothing
        (Right t) -> Just t
    where
        p = try parseTwoSkips <|> parseOneSkip

parseASkip :: String -> Maybe Integer
parseASkip skip =
    case parse (parseOneSkip <* eof) "cmp" skip of
        (Left _) -> Nothing
        (Right (t, _)) -> Just t

-- Options

data Volume = Quiet | Normal | Verbose
    deriving (Show, Eq)

data Options = Options
    { optPrintBytes :: Bool
    , optIgnore :: (Integer, Integer)
    , optLimit :: Integer
    , optVolume :: Volume
    }
    deriving (Show, Eq)

defaults :: Options
defaults = Options
    { optPrintBytes = False
    , optIgnore = (0, 0)
    , optLimit = 0
    , optVolume = Normal
    }

options :: [OptDescr (Options -> Either String Options)]
options =
    [ Option "b" ["print-bytes"]
        (NoArg
            (\opt -> Right opt { optPrintBytes = True }))
        "print differing bytes"

    , Option "l" ["verbose"]
        (NoArg
            (\opt -> Right opt { optVolume = Verbose }))
        "output byte numbers and differing values"

    , Option "s" ["quiet", "silent"]
        (NoArg
            (\opt -> Right opt { optVolume = Quiet }))
        "supress all normal output"

    , Option "n" ["bytes"]
        (ReqArg
            (\arg opt -> case parseASkip arg of
              Just n -> Right opt { optLimit = n }
              _         -> Left $ "error: '" <> arg <> "' is not a number")
            "LIMIT")
        "compare at most LIMIT bytes"

    , Option "i" ["ignore-initial"]
        (ReqArg
            (\arg opt -> case parseSkip arg of
              Just a -> Right opt { optIgnore = a }
              _      -> Left $ "error: '" <> arg <> "' is not a valid SKIP")
            "SKIP or SKIP1:SKIP2")
        "skip the first SKIP bytes from both files or set different SKIP values for each file "

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "head" options))
        "Show this help text"
    ]
