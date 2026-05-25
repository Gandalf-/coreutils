{-# LANGUAGE BangPatterns #-}

module Coreutils.Wc (Wc(..), Counts, streamCounter, countWords, summation, runner, display, pretty, Options(..), defaults) where

-- wc, word count
--
--  -l lines
--  -w words
--  -c chars

import           Control.Monad
import qualified Data.ByteString.Char8      as C
import           Data.Char                  (isSpace)
import           Data.List                  (transpose)
import           GHC.Int                    (Int64)
import           System.Console.GetOpt
import           System.Exit
import           System.IO                  (IOMode (..), withFile)

import           Streaming                  (Of (..))
import qualified Streaming.ByteString.Char8 as Q

import           Coreutils.Util

-- boilerplate

data Wc = Wc

instance Util Wc where
        run _ = wcMain

wcMain :: [String] -> IO ()
-- ^ parse arguments, do the work
wcMain args = do
        unless (null errors) $
            die $ unlines errors

        case foldM (flip id) defaults actions of
            Left err -> die err

            Right (Options False False False) ->
                -- sneak default behavior in here
                runner files >>= display (Options True True True)

            Right opts ->
                -- main path
                runner files >>= display opts
    where
        (actions, files, errors) = getOpt RequireOrder options args

-- implementation

type Counts = (Int64, Int64, Int64)

summation :: [Counts] -> Counts
-- ^ sum for Counts
summation = foldl' add (0, 0, 0)
    where
        add (!a, !b, !c) (!x, !y, !z) = (a + x, b + y, c + z)

runner :: [String] -> IO [(Counts, FilePath)]
-- ^ run the counter function for each input, report totals together for
-- pretty presentation
runner args
        | null files = (: []) <$> wc ""
        | otherwise  = mapM wc files
    where
        wc "-" = go Q.getContents "-"
        wc ""  = go Q.getContents ""
        wc path = do
            counts <- withFile path ReadMode (streamCounter . Q.fromHandle)
            return (counts, path)

        go stream n = do
            counts <- streamCounter stream
            return (counts, n)

        files = filter (/= "--") args

display :: Options -> [(Counts, FilePath)] -> IO ()
-- ^ produce a total if necessary
display o results
        | length results > 1 = pretty o $ results ++ [(total, "total")]
        | otherwise          = pretty o results
    where
        total = summation $ map fst results

pretty :: Options -> [(Counts, FilePath)] -> IO ()
-- ^ padding aware columnar display for results
pretty (Options ol ow oc) counts =
        mapM_ (putStrLn . unwords)
        $ transpose
            [ smartBuffer Righty lineCount
            , smartBuffer Righty wordCount
            , smartBuffer Righty charCount
            , smartBuffer Lefty  paths
            ]
    where
        lineCount = if ol then map ((\(a, _, _) -> show a) . fst) counts else []
        wordCount = if ow then map ((\(_, a, _) -> show a) . fst) counts else []
        charCount = if oc then map ((\(_, _, a) -> show a) . fst) counts else []
        paths = map snd counts

-- options

data Options = Options
        { optLines :: Bool
        , optWords :: Bool
        , optChars :: Bool
        }

defaults :: Options
defaults = Options False False False

options :: [OptDescr (Options -> Either String Options)]
options =
    [ Option "c" ["bytes"]
        (NoArg
            (\opt -> Right opt { optChars = True }))
        "print the byte counts"

    , Option "l" ["lines"]
        (NoArg
            (\opt -> Right opt { optLines = True }))
        "print the newline counts"

    , Option "w" ["words"]
        (NoArg
            (\opt -> Right opt { optWords = True }))
        "print the word count"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "wc" options))
        "show this help text"
    ]

-- utility

data Alignment = Lefty | Center | Righty

buffer :: Alignment -> Int -> String -> String
-- ^ pad the given string to a particular length, using an alignment scheme
buffer Righty n s = replicate (n - length s) ' ' <> s
buffer Lefty  n s = take n $ s <> cycle " "
buffer Center n s = take n body
    where
        body = pad <> s <> cycle " "
        pad = replicate ((n - length s) `div` 2) ' '

smartBuffer :: Alignment -> [String] -> [String]
-- ^ choose an appropriate buffer pad size given a list of elements so that
-- none are truncated and to limit extra output space
smartBuffer a column = map (buffer a pad) column
    where
        pad = maximum [4, maximum $ map length column]

streamCounter :: Monad m => Q.ByteStream m () -> m Counts
-- ^ single-pass streaming word/line/byte counter
streamCounter = fmap extract . Q.chunkFold step initial id
    where
        initial = (0, 0, 0, True)
        extract ((!l, !w, !b, _) :> _) = (fromIntegral l, fromIntegral w, fromIntegral b)

        step (!l, !w, !b, !sp) chunk = (l + newlines, w + words, b + bytes, sp')
            where
                bytes    = C.length chunk
                newlines = C.count '\n' chunk
                (words, sp') = countWords sp chunk

countWords :: Bool -> C.ByteString -> (Int, Bool)
-- ^ count space-to-nonspace transitions in a strict ByteString.
-- takes and returns whether the previous byte was whitespace,
-- enabling correct counting across chunk boundaries.
countWords wasSpace = C.foldl' step (0, wasSpace)
    where
        step (!n, !sp) c
            | isSpace c = (n, True)
            | sp        = (n + 1, False)
            | otherwise = (n, False)
