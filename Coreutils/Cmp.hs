{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Coreutils.Cmp where

import Control.Monad
import           GHC.Int               (Int64)
import           GHC.IO.Handle
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Text.Parsec
import           System.Console.GetOpt
import           System.Exit
import           System.IO

import           Coreutils.Util

data Cmp = Cmp

instance Util Cmp where
    run _ = cmpMain

cmpMain :: [String] -> IO ()
cmpMain = undefined

cmpIO :: Options -> [String] -> IO ()
cmpIO opts (p1: p2: s1: s2: _) =
    case parseSkip (s1 <> ":" <> s2) of
        Nothing  -> die $ "error: " <> s1 <> " is not a valid SKIP"
        (Just s) -> cmpIO opts { optIgnore = s } [p1, p2]

cmpIO opts [p1, p2, s1] =
    case parseSkip s1 of
        Nothing  -> die $ "error: " <> s1 <> " is not a valid SKIP"
        (Just s) -> cmpIO opts { optIgnore = s } [p1, p2]

cmpIO opts [p1, p2] = do
    f1 <- getFile p1
    f2 <- getFile p2
    cmp opts f1 f2

cmpIO opts [p1] = do
    f1 <- getFile p1
    f2 <- getFile "-"
    cmp opts f1 f2

cmpIO _ [] = die "missing first file"

cmp :: Options -> File -> File -> IO ()
cmp = undefined

skipBytes :: Integer -> File -> IO ()
skipBytes n (File h _ s) =
    case s of
        Nothing   -> void $ B.hGet h (fromIntegral n)
        (Just fs) -> hSeek h AbsoluteSeek (min (toInteger fs) n)

cmpBlock :: Options -> ByteString -> ByteString -> IO ()
cmpBlock = undefined

-- IO

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

-- Comparision

data Cursor = Cursor
    { cLine :: !Int
    , cByte :: !Int
    }

sCompare :: ByteString -> ByteString -> [Result]
sCompare = undefined

data Result = Same | Different

{-
wCompare :: Word8 -> Word8 -> Result
wCompare l r
    | l == r    = Same
    | otherwise = Different
-}

-- SKIP Parsing

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
    pure (s, s)

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

-- Options

data Volume = Quiet | Normal | Verbose

data Options = Options
    { optPrintBytes :: Bool
    , optIgnore :: (Integer, Integer)
    , optLimit :: Integer
    , optVolume :: Volume
    }

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
            (\arg opt -> case reads arg of
              [(n, "")] -> Right opt { optLimit = n }
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
