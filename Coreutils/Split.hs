module Coreutils.Split where

-- split
--
-- break a file into chunks by line or byte count

import           Control.Monad
import           Coreutils.Util
import           Data.Char
import           Data.List
import           System.IO
import           System.Console.GetOpt
import           System.Exit
import           Text.Read

data Split = Split

instance Util Split where
    run _ = splitMain

data Runtime = NoRuntime | RunBytes Int | RunLines Int | RunChunks Int
    deriving (Show, Eq)

data File = File Handle (Maybe Integer)

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

        file <- case files of
            [] ->
                return $ File stdin Nothing

            [fn] -> do
                h <- openBinaryFile fn ReadMode
                size <- hFileSize h
                pure $ File h (Just size)

            _ ->
                die "split cannot operate on more than one file at a time"

        case foldM (flip id) defaults actions of
            Left err   -> die err
            Right opts -> runSplit opts file

runSplit :: Options -> File -> IO ()
-- switchboard, this is where we apply the default runtime if needed too
runSplit opts files =
        case optRuntime opts of
            NoRuntime   -> runSplit (opts { optRuntime = RunLines 1000}) files
            RunBytes n  -> undefined
            RunLines n  -> undefined
            RunChunks n -> undefined

adjustment :: String -> Maybe Int
-- convert 10K  to 10240 (10 * 1024), etc
-- convert 10KB to 10000 (10 * 1000), etc
adjustment xs = case span isNumber characters of
        ("", _)       -> Nothing
        (ys, "")      -> Just $ read ys
        (ys, [v,'b']) -> (* read ys) <$> adjust v 1024
        (ys, [v])     -> (* read ys) <$> adjust v 1000
        _             -> Nothing
    where
        characters = map toLower xs

adjust :: Char -> Int -> Maybe Int
adjust n value = (\x -> value ^ (x + 1)) <$> elemIndex n "kmgtpezy"

suffixGenerator :: Bool -> Int -> String -> [String]
-- ^ lazy list of suffixes that conform to these options. an appropriate length must be
-- determined ahead of time so that enough suffixes will be available
suffixGenerator numeric width extra =
        map (<> extra) $ replicateM width characters
    where
        characters
            | numeric   = concatMap show ([0..9] :: [Integer])
            | otherwise = ['a'..'z']

defaults :: Options
defaults = Options
        { optSuffixLength = 2
        , optExtraSuffix  = ""
        , optNumeric      = False
        , optRuntime      = NoRuntime
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
                (Just n) -> Right opt { optRuntime = RunChunks n})
            "CHUNKS")
        "generate CHUNKS output files"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "split" options))
        "Show this help text"
    ]
