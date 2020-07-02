{-# LANGUAGE LambdaCase        #-}

module Coreutils.Shuf where

import           Control.Monad
import           GHC.Int                    (Int64)
import           System.Console.GetOpt
import           System.Exit
import           System.IO

import           Coreutils.Util

data Shuf = Shuf

instance Util Shuf where
    run _ = shufMain

data Runtime = RunRange (Int, Int) | RunFile

data Options = Options
        { optEcho   :: Bool
        , optRuntime  :: Runtime
        , optHead   :: Maybe Int
        , optOutput :: Maybe FilePath
        , optSource :: Maybe FilePath
        , optRepeat :: Bool
        }

data File = File
        { _handle   :: Handle
        , _filename :: FilePath
        , _filesize :: Maybe Int64
        }

shufMain :: [String] -> IO ()
shufMain args = do
        let (actions, arguments, errors) = getOpt RequireOrder options args

        unless (null errors) $ do
            mapM_ putStr errors
            exitFailure

        case foldM (flip id) defaults actions of
            Left   err -> die err
            Right opts ->
                case optRuntime opts of
                    RunRange r -> runRangeShuf opts r
                    _          -> runfileShuf opts arguments


runRangeShuf :: Options -> (Int, Int) -> IO ()
runRangeShuf o (l, h) = undefined


runfileShuf :: Options -> [FilePath] -> IO ()
runfileShuf _ filenames = do
        files <- case filenames of
            [] -> (: []) <$> getFile "-"
            fs -> mapM getFile fs
        pure ()


getFile :: FilePath -> IO File
-- take the filename, acquire a handle and determine it's size
getFile "-" = pure $ File stdin "-" Nothing
getFile name = do
    h <- openBinaryFile name ReadMode
    size <- hIsSeekable h >>= \case
        True  -> Just . fromIntegral <$> hFileSize h
        False -> pure Nothing
    pure $ File h name size

-- | Options

readRange :: String -> Maybe (Int, Int)
readRange = undefined

defaults :: Options
defaults = Options
        { optEcho = False
        , optRuntime = RunFile
        , optHead = Nothing
        , optOutput = Nothing
        , optSource = Nothing
        , optRepeat = False
        }

options :: [OptDescr (Options -> Either String Options)]
options =
    [ Option "e" ["echo"]
        (NoArg
            (\opt -> Right opt { optEcho = True }))
        "treat each argument as an input line"

    , Option "i" ["input-range"]
        (ReqArg
            (\arg opt -> case readRange arg of
              Just r -> Right opt { optRuntime = RunRange r }
              _      -> Left $ "error: '" <> arg <> "' is not a range")
            "LO-HI")
        "treat each number LO through HI as input lines"

    , Option "n" ["head-count"]
        (ReqArg
            (\arg opt -> case reads arg of
              [(n, "")] -> Right opt { optHead = n }
              _         -> Left $ "error: '" <> arg <> "' is not a number")
            "COUNT")
        "output at most COUNT lines"

    , Option "o" ["output"]
        (ReqArg
            (\arg opt -> Right opt { optOutput = Just arg})
            "FILE")
        "write results to FILE instead of stdout"

    , Option "" ["random-source"]
        (ReqArg
            (\arg opt -> Right opt { optSource = Just arg})
            "FILE")
        "get random bytes from FILE"

    , Option "r" ["repeat"]
        (NoArg
            (\opt -> Right opt { optRepeat = True }))
        "output lines can be repeated"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "head" options))
        "Show this help text"
    ]
