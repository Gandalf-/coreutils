{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Coreutils.Uniq where

-- uniq
--
-- where the ordering of lines is completely ignored
-- equivalent to sort | uniq
-- supports the majority of the GNU util's options
--
-- internally, we use a hashmap to count lines after they've been processed
-- flags either control output, filtering before output, or modification
-- before input

import           Data.Char        (isSpace, toLower)
import           Data.List        (sortOn)
import qualified Data.Map.Strict  as Map
import           Data.Maybe       (fromMaybe)
import           Data.Ord         (Down (..))
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as C
import           System.Directory (doesFileExist)
import           System.Exit      (die)
import           Text.Printf      (printf)
import           System.Console.GetOpt

import           Coreutils.Util
import Control.Monad.State.Strict
import qualified Streaming.ByteString.Char8 as Q
import qualified Streaming.Prelude as S
import Streaming

helpText :: String
helpText = concat
    ["uniq [option ...] [files ...]"
    ,""
    ,"  -h     show this help"
    ,"  -c     prefix lines with the number of occurences"
    ,"  -u     only show unique lines"
    ,"  -d     only show repeated lines"
    ,"  -i     do not consider case while making a match"
    ,"  -s n   do not compare the first n characters"
    ,"  -w n   only compare the first n characters"
    ]

type WordMap = Map.Map String Integer


singleton :: WordMap
singleton = Map.empty


increment :: WordMap -> String -> WordMap
increment wm word =
      Map.alter add word wm
  where
      add :: Maybe Integer -> Maybe Integer
      add Nothing  = Just 1
      add (Just i) = Just $ i + 1


mutateContent :: [Flag] -> String -> [String]
mutateContent flags content =
      case flags of
        (Ignore  : fs) -> mutateContent fs $ map toLower content
        (Skip  n : fs) -> mutateContent fs $ apply (drop n) content
        (First n : fs) -> mutateContent fs $ apply (take n) content
        (_       : fs) -> mutateContent fs content
        []             -> map (dropWhile isSpace) $ lines content
  where
      apply f ls = unlines $ map f $ lines ls


mutateMap :: [Flag] -> WordMap -> WordMap
mutateMap flags wm
      | Unique' `elem` flags = Map.filter (== 1) wm
      | Repeat  `elem` flags = Map.filter (/= 1) wm
      | otherwise           = wm


data Flag = Count | Ignore | Unique' | Repeat | Skip Int | First Int
    deriving (Eq)


countWords :: [Flag] -> String -> IO ()
countWords flags content
      | Count `elem` flags  = mapM_ printCount list
      | otherwise           = mapM_ printPlain list
  where
      list = extract $ mutateMap flags wm
      wm = foldl increment singleton $ mutateContent flags content

      extract :: WordMap -> [(String, Integer)]
      extract = sortOn (Down . snd) . Map.toList

      printCount (s, i) = putStrLn $ printf "% 5d %s" i s
      printPlain (s, _) = putStrLn s


handle :: [Flag] -> [String] -> Maybe String -> IO ()
handle flags arguments content =
      case arguments of
        ("-h" : _   ) -> putStrLn helpText

        ("-c" : args) -> handle (Count  : flags) args content
        ("-u" : args) -> handle (Unique' : flags) args content
        ("-d" : args) -> handle (Repeat : flags) args content
        ("-i" : args) -> handle (Ignore : flags) args content

        ("-s" : n : args) ->
            handle (Skip (read n :: Int) : flags) args content

        ("-w" : n : args) ->
            handle (First (read n :: Int) : flags) args content

        (file : args) -> do
            exists <- doesFileExist file
            if exists
              then do
                fileContent <- readFile file
                handle flags args $ Just (fileContent ++ fromMaybe "" content)

              else die $
                "uniq: " ++ file ++ ": No such file or directory"

        [] -> output content
  where
      output :: Maybe String -> IO ()
      output Nothing  = getContents >>= countWords flags
      output (Just c) = countWords flags c


data Uniq = Uniq

instance Util Uniq where
    run _ args = handle [] args Nothing

-- | IO

type Op = StateT UniqState IO

unique :: Options -> Q.ByteStream Op () -> IO ()
unique os bs = do
        (_, st) <- worker Q.stdout bs initial
        C.putStr $ finalize st
    where
        initial = getState $ getRuntime os

worker :: (Q.ByteStream Op () -> Op a) -> Q.ByteStream Op () -> UniqState -> IO (a, UniqState)
worker sink bs = runStateT (sink $ go bs)
    where
        go = Q.unlines . Q.denull . S.subst Q.chunk
           . S.mapM process
           . mapped Q.toStrict . Q.lines

process :: Line -> Op Line
process l = do
    st <- get
    let (!new, !line) = execute st l
    put new
    return line

-- | Implementation

type Line = ByteString

data UniqState = UniqState {
      count    :: !Int
    , previous :: !Line
    , runtime  :: !Runtime
}

execute :: UniqState -> Line -> (UniqState, Line)
execute st line
    | match rt same n = (newState, format rt n prev)
    | otherwise       = (newState, C.empty)
    where
        newState
            | same      = st { previous = line, count = n + 1 }
            | otherwise = st { previous = line, count = 1}
        same = prep line == prep prev

        rt = runtime st
        n = count st
        prep = prepare rt
        prev = previous st

finalize :: UniqState -> Line
finalize st
    | final (runtime st) (count st) = previous st <> "\n"
    | otherwise = C.empty

getState :: Runtime -> UniqState
getState rt = UniqState {
      count = 0
    , previous = C.empty
    , runtime = rt
    }

data Runtime = Runtime {
    -- count
      format  :: Int -> Line -> Line
    -- skip fields, skip chars, case insensitive
    , prepare :: Line -> Line
    -- same -> count -> print previous?
    , match   :: Bool -> Int -> Bool
    -- count -> print previous?
    , final   :: Int -> Bool
}

getRuntime :: Options -> Runtime
getRuntime os = Runtime {
          format = formatter
        , prepare = prepper
        , match = getMatcher
        , final = finalizer
        }
    where
        getMatcher
            | optUnique os      = matcher Unique
            | optRepeated os    = matcher Repeat1
            | optAllRepeated os = matcher RepeatA
            | otherwise         = matcher Dedupe

        finalizer nPrev
            | optUnique os      = nPrev == 1
            | optRepeated os    = nPrev > 1
            | optAllRepeated os = nPrev > 1
            | otherwise         = True

        prepper line
            | optIgnoreCase os = C.map toLower line
            -- TODO
            | otherwise        = line

        formatter n line
            | optCount os = C.pack (show n) <> " " <> line
            | otherwise   = line

data Matcher = Unique | Repeat1 | RepeatA | Dedupe

matcher :: Matcher -> Bool -> Int -> Bool
matcher Unique  same n = not same && n == 1
matcher Repeat1 same n = not same && n > 1
matcher RepeatA same n = same || n > 1
matcher Dedupe  same n = not same && n /= 0

-- | Options

data Options = Options {
      optCount :: Bool

    , optRepeated :: Bool
    , optAllRepeated :: Bool
    , optUnique :: Bool

    , optSkipFields :: Int
    , optSkipChars :: Int
    , optIgnoreCase :: Bool
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
    [ Option "c" ["count"]
        (NoArg
            (\opt -> Right opt { optCount = True }))
        "Precede each output line with an occurrence count"

    , Option "d" ["repeated"]
        (NoArg
            (\opt -> Right opt { optRepeated = True }))
        "Output a single copy of each repeated line"

    , Option "D" ["all-repeated"]
        (NoArg
            (\opt -> Right opt { optAllRepeated = True }))
        "Output all lines that are repeated"

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

    , Option "i" ["ignore-case"]
        (NoArg
            (\opt -> Right opt { optIgnoreCase = True }))
        "Case insenstive comparision of lines"

    , Option "u" ["unique"]
        (NoArg
            (\opt -> Right opt { optUnique = True }))
        "Output all lines that are not repeated"

    , Option "" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "awk" optionDesc))
        "Show this help text"
    ]
