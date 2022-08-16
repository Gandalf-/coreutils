{-# LANGUAGE OverloadedStrings #-}
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
import Control.Monad

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
      | Unique `elem` flags = Map.filter (== 1) wm
      | Repeat `elem` flags = Map.filter (/= 1) wm
      | otherwise           = wm


data Flag = Count | Ignore | Unique | Repeat | Skip Int | First Int
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
        ("-u" : args) -> handle (Unique : flags) args content
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

-- | Implementation

type Line = ByteString

data UniqState = UniqState {
      count :: Int
    , previous :: Line
    , runtime :: Runtime
}

execute :: UniqState -> Line -> (UniqState, Line)
execute st line =
    case match rt prev nPrev line of
        Nothing  -> (newState, C.empty)
        Just out -> (newState, format rt nPrev out)
    where
        rt = runtime st
        nPrev = count st
        prev = previous st
        newState
            | line == prev = st { previous = line, count = nPrev + 1 }
            | otherwise    = st { previous = line, count = 1}

getState :: Runtime -> UniqState
getState rt = UniqState {
      count = 0
    , previous = C.empty
    , runtime = rt
    }

data Runtime = Runtime {
    -- count
      format :: Int -> Line -> Line
    -- skip fields, skil chars, case insensitive
    , prepare :: Line -> Line
    -- previous -> count -> current
    , match :: Line -> Int -> Line -> Maybe Line
    , final :: Int -> Bool
}

getRuntime :: Options -> Runtime
getRuntime os = Runtime {
          format = formatter
        , prepare = prepper
        , match = matcher
        , final = finaler
        }
    where
        matcher
            | optUnique os      = uniqueMatcher
            | optRepeated os    = repeatMatcher
            | optAllRepeated os = allRepeatMatcher
            | otherwise         = dedupeMatcher

        uniqueMatcher prev nPrev curr = do
            guard (prev /= curr && nPrev == 1)
            return prev
        repeatMatcher prev nPrev curr = do
            guard (prev /= curr && nPrev > 1)
            return prev
        allRepeatMatcher prev nPrev curr = do
            guard (prev == curr || nPrev > 1)
            return prev
        dedupeMatcher prev nPrev curr = do
            guard (prev /= curr && nPrev /= 0)
            return prev

        finaler nPrev
            | optUnique os      = nPrev == 1
            | optRepeated os    = nPrev > 1
            | optAllRepeated os = nPrev > 1
            | otherwise         = True

        prepper line
            | optIgnoreCase os = C.map toLower line
            | otherwise        = line

        formatter n line
            | optCount os = C.pack (show n) <> " " <> line
            | otherwise   = line

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
