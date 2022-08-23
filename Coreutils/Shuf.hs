{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-matches #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}

module Coreutils.Shuf where

import           Control.Monad
import           GHC.Int               (Int64)
import           GHC.IO.Handle
import           System.Console.GetOpt
import           System.Exit
import           System.IO
import           System.Random
import           System.Random.Shuffle

import           Coreutils.Util

data Shuf = Shuf

instance Util Shuf where
    run _ = shufMain

data Runtime = RunRange (Int, Int) | RunFile | RunEcho

data Options = Options
        { optRuntime :: Runtime
        , optHead    :: Maybe Int
        , optOutput  :: Maybe FilePath
        , optSource  :: Maybe FilePath
        , optRepeat  :: Bool
        }

data File = File
        { _handle   :: Handle
        , _filename :: FilePath
        , _filesize :: Maybe Int64
        }

-- | Business logic
simpleShuf :: StdGen -> [String] -> [String]
simpleShuf g xs = shuffle' xs l g
    where
        l = length xs

-- | IO
shufMain :: [String] -> IO ()
shufMain args = do
        let (actions, arguments, errors) = getOpt RequireOrder options args

        unless (null errors) $ do
            mapM_ putStr errors
            exitFailure
        g <- getStdGen

        case foldM (flip id) defaults actions of
            Left   err -> die err
            Right opts ->
                setupOutput opts

data ShufBox = forall s. Shuffler s => SB s

getShuffler :: Options -> [String] -> ShufBox
getShuffler o args =
    case optRuntime o of
        RunEcho         -> SB $ LineShuf args
        RunRange (l, h) -> SB $ RangeShuf l h
        RunFile         -> undefined

class Shuffler a where
    shuf :: a -> StdGen -> [String]
    validate :: a -> Maybe String

runRangeShuf :: Options -> (Int, Int) -> IO ()
runRangeShuf o (l, h) = undefined

runArgShuf :: Options -> [String] -> IO ()
runArgShuf = runFileShuf

runFileShuf :: Options -> [FilePath] -> IO ()
runFileShuf _ filenames = do
        file <- case filenames of
            []    -> getFile "-"
            (f:_) -> getFile f
        pure ()

runShuf :: Shuffler a => a -> Options -> [String]
runShuf a o = undefined
    where
        limiter = case optHead o of
            Nothing  -> id
            (Just l) -> take l

data RangeShuf = RangeShuf Int Int

instance Shuffler RangeShuf where
    shuf (RangeShuf l h) =
            map show . shuffle' vs size
        where
            vs = [l..h]
            size = h - l + 1

    validate (RangeShuf l h)
        | l <= h = Nothing
        | otherwise = Just "Invalid range"


newtype LineShuf = LineShuf [String]

instance Shuffler LineShuf where
    shuf (LineShuf xs) = shuffle' xs size
        where
            size = length xs

    validate _ = Nothing


setupOutput :: Options -> IO ()
-- maybe change where stdout points so we can just print everywhere else
setupOutput o = case optOutput o of
    Nothing  -> pure ()
    (Just f) -> do
        h <- openFile f WriteMode
        hDuplicateTo h stdout

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
        { optRuntime = RunFile
        , optHead = Nothing
        , optOutput = Nothing
        , optSource = Nothing
        , optRepeat = False
        }

options :: [OptDescr (Options -> Either String Options)]
options =
    [ Option "e" ["echo"]
        (NoArg
            (\opt -> Right opt { optRuntime = RunEcho }))
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
