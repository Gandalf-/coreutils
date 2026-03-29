{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Coreutils.Uniq where

-- uniq
--
-- Functionally equivalent to BSD uniq except for nonsense option combinations
-- like --unique --repeated

import           Control.Monad              (foldM, unless)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as C
import           Data.Char                  (isDigit, toLower)
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

unique :: Options -> Q.ByteStream IO () -> IO ()
unique os bs = do
        (st, _) <- Q.stdout $ Q.unlines $ S.subst Q.chunk
                  $ S.for inner S.each
        C.putStr $ C.concat (finalize st)
    where
        inner = mapAccum execute initial $ mapped Q.toStrict $ Q.lines bs
        initial = getState $ getRuntime os

mapAccum :: Monad m => (s -> a -> (s, b)) -> s -> Stream (Of a) m r -> Stream (Of b) m (s, r)
mapAccum step = loop
  where
    loop !s str = lift (S.next str) >>= \case
        Left r          -> pure (s, r)
        Right (a, rest) -> let (!s', b) = step s a
                           in S.yield b >> loop s' rest

-- | Implementation

type Line = ByteString
type Prepped = ByteString

data UniqState = UniqState {
      count    :: Int
    , previous :: Maybe (Prepped, Line)
    , leader   :: Maybe Line
    , groupNum :: Int
    , runtime  :: Runtime
}

execute :: UniqState -> Line -> (UniqState, [Line])
execute st line
    | match rt same n = (newState { groupNum = newGroupNum }, sep <> emit st)
    | otherwise       = (newState, [])
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

        -- A new repeated group starts when we first detect repetition
        groupStart = same && n == 1
        newGroupNum
            | groupStart = groupNum st + 1
            | otherwise  = groupNum st
        sep = separator (sepType rt) newGroupNum groupStart

        prepped = prepare rt line
        same = (Just prepped ==) $ fst <$> previous st

        rt = runtime st
        n = count st

emit :: UniqState -> [Line]
emit st = maybe [] (\l -> [format rt n l]) (emitLine rt st)
    where
        rt = runtime st
        n = count st

separator :: Maybe SepType -> Int -> Bool -> [Line]
separator (Just SepPrepend)  _ True         = [""]
separator (Just SepSeparate) g True | g > 1 = [""]
separator _                  _ _            = []

finalize :: UniqState -> [Line]
finalize st
    | emitFinal (runtime st) (count st) = map (<> "\n") (emit st)
    | otherwise = []

getState :: Runtime -> UniqState
getState rt = UniqState {
      count = 0
    , previous = Nothing
    , leader = Nothing
    , groupNum = 0
    , runtime = rt
    }

data Runtime = Runtime {
      format    :: Int -> Line -> Line
    , prepare   :: Line -> Line
    , match     :: Bool -> Int -> Bool
    , emitFinal :: Int -> Bool
    , emitLine  :: UniqState -> Maybe Line
    , sepType   :: Maybe SepType
}

getRuntime :: Options -> Runtime
getRuntime os = Runtime { .. }
    where
        useAllRepeated = isJust (optAllRepeated os)

        match
            | optUnique os   = matcher Unique
            | useAllRepeated = matcher RepeatA
            | optRepeated os = matcher Repeat1
            | otherwise      = matcher Dedupe

        emitFinal nPrev
            | optUnique os   = nPrev == 1
            | useAllRepeated = nPrev > 1
            | optRepeated os = nPrev > 1
            | otherwise      = True

        emitLine st
            | useAllRepeated = snd <$> previous st
            | otherwise      = leader st

        sepType = optAllRepeated os

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

data SepType = SepNone | SepPrepend | SepSeparate
    deriving (Show, Eq)

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
    , optAllRepeated :: Maybe SepType
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
    , optAllRepeated = Nothing

    , optSkipFields = 0
    , optSkipChars = 0
    , optIgnoreCase = False
}

getInt :: String -> Either String Int
getInt s
    | not (null s) && all isDigit s = Right $ read s
    | otherwise                     = Left $ s <> " is not a number"

parseSeparator :: Maybe String -> Either String SepType
parseSeparator Nothing           = Right SepNone
parseSeparator (Just "none")     = Right SepNone
parseSeparator (Just "prepend")  = Right SepPrepend
parseSeparator (Just "separate") = Right SepSeparate
parseSeparator (Just s)          = Left $ "unknown separator type: " <> s

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
        (OptArg
            (\arg opt -> (\s -> opt { optAllRepeated = Just s }) <$> parseSeparator arg)
            "septype")
        "Output all lines that are repeated (none, prepend, separate)"

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
