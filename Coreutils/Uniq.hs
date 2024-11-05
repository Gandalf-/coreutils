{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Coreutils.Uniq where

-- uniq
--
-- Functionally equivalent to BSD uniq except for nonsense option combinations
-- like --unique --repeated

import           Control.Monad              (foldM, unless)
import           Control.Monad.State.Strict
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as C
import           Data.Char                  (toLower)
import           Streaming
import qualified Streaming.ByteString.Char8 as Q
import qualified Streaming.Prelude          as S
import           System.Console.GetOpt
import           System.Exit
import           System.IO

import           Coreutils.Util
import           Data.Maybe

data Uniq = Uniq

instance Util Uniq where
    run _ = uniqMain

-- | IO

uniqMain :: [String] -> IO ()
uniqMain args = do
        unless (null errors) $
            die $ unlines errors
        either die (`runUniq` other) $
            foldM (flip id) defaultOptions opts
    where
        (opts, other, errors) = getOpt RequireOrder optionDesc args

runUniq :: Options -> [String] -> IO ()
runUniq os [] = runUniq os ["-"]
runUniq os fs = mapM_ runner fs
    where
        runner :: FilePath -> IO ()
        runner "-" = unique os Q.stdin
        runner f   = withFile f ReadMode (unique os . Q.fromHandle)

type Op = StateT UniqState IO

unique :: Options -> Q.ByteStream Op () -> IO ()
unique os bs = do
        (_, st) <- worker Q.stdout bs initial
        C.putStr $ fromMaybe C.empty (finalize st)
    where
        initial = getState $ getRuntime os

worker :: (Q.ByteStream Op () -> Op a) -> Q.ByteStream Op () -> UniqState -> IO (a, UniqState)
worker sink bs = runStateT (sink $ go bs)
    where
        go = Q.unlines . S.subst Q.chunk
           . S.mapMaybeM process
           . mapped Q.toStrict . Q.lines

process :: Line -> Op (Maybe Line)
process l = do
    st <- get
    let (!new, !line) = execute st l
    put new
    return line

-- | Implementation

type Line = ByteString
type Prepped = ByteString

data UniqState = UniqState {
      count    :: Int
    , previous :: Maybe (Prepped, Line)
    , leader   :: Maybe Line
    , runtime  :: Runtime
}

execute :: UniqState -> Line -> (UniqState, Maybe Line)
execute st line
    | match rt same n = (newState, emit st)
    | otherwise       = (newState, Nothing)
    where
        newState = st
            { previous = Just (prepped, line)
            , leader = newLeader
            , count = newCount
            }
        newCount
            | same      = n + 1
            | otherwise = 1
        newLeader
            | isNothing (leader st) = Just line
            | same                  = leader st
            | otherwise             = Just line

        prepped = prepare rt line -- We'll only prepare each line once
        same = (Just prepped ==) $ fst <$> previous st

        rt = runtime st
        n = count st

emit :: UniqState -> Maybe Line
emit st = format rt n <$> emitLine rt st
    where
        rt = runtime st
        n = count st

finalize :: UniqState -> Maybe Line
finalize st
    | emitFinal (runtime st) (count st) = (<> "\n") <$> emit st
    | otherwise = Nothing

getState :: Runtime -> UniqState
getState rt = UniqState {
      count = 0
    , previous = Nothing
    , leader = Nothing
    , runtime = rt
    }

data Runtime = Runtime {
      format    :: Int -> Line -> Line
    , prepare   :: Line -> Line
    , match     :: Bool -> Int -> Bool
    , emitFinal :: Int -> Bool
    , emitLine  :: UniqState -> Maybe Line
}

getRuntime :: Options -> Runtime
getRuntime os = Runtime { .. }
    where
        match
            | optUnique os      = matcher Unique
            | optRepeated os    = matcher Repeat1
            | optAllRepeated os = matcher RepeatA
            | otherwise         = matcher Dedupe

        emitFinal nPrev
            | optUnique os      = nPrev == 1
            | optRepeated os    = nPrev > 1
            | optAllRepeated os = nPrev > 1
            | otherwise         = True

        emitLine st
            | optAllRepeated os = snd <$> previous st
            | otherwise         = leader st

        prepare = preparer os

        format n line
            | optCount os = C.concat [buffer, count, " ", line]
            | otherwise   = line
            where
                count  = C.pack $ show n
                buffer = C.replicate (4 - C.length count) ' '

preparer :: Options -> ByteString -> ByteString
-- ^ Prepare a line for comparison
preparer os =
        -- This appears to be the order that BSD uniq uses
        lower . fields . chars
    where
        lower
            | optIgnoreCase os = C.map toLower
            | otherwise        = id
        chars = C.drop (optSkipChars os)
        fields
            | optSkipFields os /= 0 =
                C.unwords . drop (optSkipFields os) . C.words
            | otherwise = id

data Matcher = Unique | Repeat1 | RepeatA | Dedupe

matcher :: Matcher -> Bool -> Int -> Bool
matcher Unique  same n = not same && n == 1
matcher Repeat1 same n = not same && n > 1
matcher RepeatA same n = same || n > 1
matcher Dedupe  same n = not same && n /= 0

-- | Options

data Options = Options {
      optCount       :: Bool

    , optRepeated    :: Bool
    , optAllRepeated :: Bool
    , optUnique      :: Bool

    , optSkipFields  :: Int
    , optSkipChars   :: Int
    , optIgnoreCase  :: Bool
    }
    deriving (Show, Eq)

defaultOptions :: Options
defaultOptions = Options {
      optCount = False

    , optRepeated = False
    , optUnique = False
    , optAllRepeated = False

    , optSkipFields = 0
    , optSkipChars = 0
    , optIgnoreCase = False
}

getInt :: String -> Either String Int
getInt = undefined

optionDesc :: [OptDescr (Options -> Either String Options)]
optionDesc =
    [ Option "u" ["unique"]
        (NoArg
            (\opt -> Right opt { optUnique = True }))
        "Output all lines that are not repeated"

    , Option "d" ["repeated"]
        (NoArg
            (\opt -> Right opt { optRepeated = True }))
        "Output a single copy of each repeated line"

    , Option "D" ["all-repeated"]
        (NoArg
            (\opt -> Right opt { optAllRepeated = True }))
        "Output all lines that are repeated"

    , Option "i" ["ignore-case"]
        (NoArg
            (\opt -> Right opt { optIgnoreCase = True }))
        "Case insenstive comparision of lines"

    , Option "f" ["skip-fields"]
        (ReqArg
            (\arg opt -> (\f -> opt { optSkipFields = f }) <$> getInt arg)
            "NUM")
        "Ignore the first NUM fields in each line"

    , Option "s" ["skip-chars"]
        (ReqArg
            (\arg opt -> (\f -> opt { optSkipChars = f }) <$> getInt arg)
            "CHARS")
        "Ignore the first CHARS characters in each line, after any fields"

    , Option "c" ["count"]
        (NoArg
            (\opt -> Right opt { optCount = True }))
        "Precede each output line with an occurrence count"

    , Option "" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "awk" optionDesc))
        "Show this help text"
    ]
