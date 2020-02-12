module Coreutils.Seq where

-- seq
--
-- produce a sequence of numbers with eccentric formatting behavior

import           Control.Monad
import           Data.Maybe            (catMaybes, isJust)
import           System.Console.GetOpt
import           System.Exit
import           Text.Printf           (printf)
import           Text.Read             (readMaybe)

import           Coreutils.Util

data Seq = Seq

instance Util Seq where
    run _ = seqMain

seqMain :: [String] -> IO ()
seqMain args = do
        let (actions, nums, errors) = getOpt RequireOrder options args

        unless (null errors) $ do
            mapM_ putStr errors
            exitFailure

        case foldM (flip id) defaults actions of
            Left   err -> die err
            Right opts ->
                case getSeqType nums of
                    (Decimals width) -> do
                        let arguments = map readMaybe nums
                        case runSeq $ catMaybes arguments of
                            (Left  err)     -> die $ "seq: " <> err
                            (Right numbers) -> display dPrintf width opts numbers

                    Ints -> do
                        let arguments = map readMaybe nums
                        case runSeq $ catMaybes arguments of
                            (Left  err)     -> die $ "seq: " <> err
                            (Right numbers) -> display iPrintf (0 :: Int) opts numbers
    where
        display p w (Options _ _ t) [n]      = p ("%." <> show w <> "v" <> t) n
        display p w o@(Options _ s _) (n:ns) = p ("%." <> show w <> "v" <> s) n >> display p w o ns
        display _ _ _ []                       = return ()

data SeqType = Ints | Decimals Int
    deriving (Show)

getSeqType :: [String] -> SeqType
getSeqType [_, i, _]
        | integerAble = Ints
        | doubleAble  = Decimals decimals
        | otherwise   = Ints
    where
        integerAble = isJust (readMaybe i :: Maybe Integer)
        doubleAble = isJust (readMaybe i :: Maybe Double)

        decimals = length (dropWhile (/= '.') i) - 1

getSeqType _ = Ints

dPrintf :: String -> Double -> IO ()
dPrintf = printf

iPrintf :: String -> Integer -> IO ()
iPrintf = printf

runSeq :: (Enum a, Ord a, Num a) => [a] -> Either String [a]
-- ^ produce a list of numbers, use pattern matching to represent the
-- default values and error handling

runSeq [start, increment, end]
        | start > end && increment > 0 = Left "increment must be negative for reverse ranges"
        | otherwise                    = Right [start, start + increment .. end]

runSeq [start, end]
        | start < end = runSeq [start,  1, end]
        | otherwise   = runSeq [start, -1, end]

runSeq [end]                   = runSeq [1, 1, end]
runSeq _                       = Left "unable to parse arguments as numbers"

data Options = Options
        { optFormat     :: String
        , optSeparator  :: String
        , optTerminator :: String
        }

defaults :: Options
defaults = Options
        { optFormat     = "%v"
        , optSeparator  = "\n"
        , optTerminator = "\n"
        }

options :: [OptDescr (Options -> Either String Options)]
options =
    [ Option "f" ["format"]
        (ReqArg
            (\arg opt -> Right opt { optFormat = arg })
            "FORMAT")
        "provide the printf format to use while printing"

    , Option "s" ["separator"]
        (ReqArg
            (\arg opt -> Right opt { optSeparator = arg})
            "SEPARATOR")
        "use this string between numbers"

    , Option "t" ["terminator"]
        (ReqArg
            (\arg opt -> Right opt { optTerminator = arg})
            "TERMINATOR")
        "use this string to terminate the numbers"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "head" options))
        "Show this help text"
    ]
