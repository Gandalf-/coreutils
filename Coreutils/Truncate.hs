module Coreutils.Truncate where

import           Control.Monad
import           Coreutils.Util
import           Data.Bits
import           Data.Char
import           System.Console.GetOpt
import           System.Directory
import           System.Exit
import           System.IO

data Truncate = Truncate

instance Util Truncate where
    run _ = truncateMain

data Size =
      Absolute   Integer
    | Adjustment Integer
    | RoundUp    Integer
    | RoundDown  Integer
    | Reference  FilePath
    | None
    deriving (Eq, Show)

data Options = Options
    { optCreate :: Bool
    , optSize   :: Size
    }

-- | IO

truncateMain :: [String] -> IO ()
truncateMain args = do
        unless (null errors) $
            die $ unlines errors
        either die (`executes` arguments) $
            foldM (flip id) defaultOptions opts
    where
        (opts, arguments, errors) = getOpt RequireOrder options args

executes :: Options -> [FilePath] -> IO ()
executes (Options _ None) _  =
    die help
executes os@(Options _ (Reference path)) fs = do
    size <- withFile path ReadMode hFileSize
    executes os {optSize = Absolute size} fs
executes os fs = mapM_ (execute os) fs

execute :: Options -> FilePath -> IO ()
execute (Options create (Absolute n)) path = do
    exists <- doesFileExist path
    when (create || exists) $
        withFile path WriteMode (`hSetFileSize` value)
    where
        value = max 0 n
execute os@(Options _ size) path = do
    current <- withFile path ReadMode hFileSize
    execute os {optSize = Absolute $ adjust size current} path

adjust :: Size -> Integer -> Integer
adjust (Adjustment n) size = size + n
adjust (RoundUp 0) _       = 0
adjust (RoundUp n) size    = ceilDiv size n * n
adjust (RoundDown 0) _     = 0
adjust (RoundDown n) size  = floorDiv size n * n
adjust _ _                 = undefined

ceilDiv :: Integer -> Integer -> Integer
ceilDiv a b = ceiling (fromIntegral a / fromIntegral b :: Double)

floorDiv :: Integer -> Integer -> Integer
floorDiv a b = floor (fromIntegral a / fromIntegral b :: Double)

-- | Parsing

parseSize :: String -> Either String Size
parseSize ('+':xs) = Adjustment <$> parseValue xs
parseSize ('-':xs) = Adjustment . negate <$> parseValue xs
parseSize ('%':xs) = RoundUp <$> parseValue xs
parseSize ('/':xs) = RoundDown <$> parseValue xs
parseSize xs       = Absolute <$> parseValue xs

parseSuffix :: String -> Either String Integer
parseSuffix ""  = Right $ bit 0
parseSuffix "k" = Right $ bit 10
parseSuffix "m" = Right $ bit 20
parseSuffix "g" = Right $ bit 30
parseSuffix "t" = Right $ bit 40
parseSuffix xs  = Left $ xs <> " is not a valid byte suffix"

parseValue :: String -> Either String Integer
parseValue xs
    | null digits = Left "No size provided"
    | otherwise   = (*) <$> value <*> parseSuffix (map toLower suffix)
    where
        (digits, suffix) = span isDigit xs
        value = Right (read digits)

-- | Options

defaultOptions :: Options
defaultOptions = Options True None

help :: String
help = usageInfo "truncate [OPTIONS] file ..." options

options :: [OptDescr (Options -> Either String Options)]
options =
    [ Option "c" []
        (NoArg
            (\opt -> Right opt { optCreate = False }))
        "Do not create files if they do not exist. This isn't treated as an error"

    , Option "r" []
        (ReqArg
            (\arg opt -> Right opt { optSize = Reference arg })
            "rfile")
        "Truncate or extend files to the length of `rfile`"

    , Option "s" []
        (ReqArg
            (\arg opt -> (\s -> opt { optSize = s }) <$> parseSize arg)
            "width")
        "Line width to use instead of the default of 80"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left help))
        "Show this help text"
    ]
