{-# LANGUAGE BangPatterns #-}

module Coreutils.Wc where

-- wc, word count
--
--  -l lines
--  -w words
--  -c chars

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.List                  (foldl', transpose)
import           GHC.Int                    (Int64)
import           System.Console.GetOpt
import           System.Exit

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
--
-- build a strict accumulation of counts over a lazy iteration of the input

type Counts = (Int64, Int64, Int64)

counter :: L.ByteString -> Counts
-- ^ accummulate totals from every line in the input stream
counter = summation . map accumulate . L.lines
    where
        accumulate !s = (1, _words, _chars)
            where
                _chars = 1 + L.length s
                _words = fromIntegral $! length $! L.words s

summation :: [Counts] -> Counts
-- ^ sum for Counts
summation = foldl' add (0, 0, 0)
    where
        add (!a, !b, !c) (!x, !y, !z) = (a + x, b + y, c + z)

runner :: [String] -> IO [(Counts, FilePath)]
-- ^ run the counter function for each input, report totals together for
-- pretty presentation
runner args
        | null files = (: []) <$> go L.getContents ""
        | otherwise  = mapM wc files
    where
        wc "-"  = go L.getContents "-"
        wc path = go (L.readFile path) path

        go f n = do
            counts <- counter <$> f
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
