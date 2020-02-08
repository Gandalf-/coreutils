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

data Wc = Wc

instance Util Wc where
        run _ = wcMain

type Counts = (Int64, Int64, Int64)

totals :: L.ByteString -> Counts
-- accummulate totals from every line in the input stream
totals = foldl' summation (0, 0, 0) . map accumulate . L.lines

summation :: Counts -> Counts -> Counts
-- strict addition
summation (!a, !b, !c) (!x, !y, !z) = (a + x, b + y, c + z)

accumulate :: L.ByteString -> Counts
-- extract the line count, words, and byte count for a single line
accumulate !s = (1, _words, _chars)
    where
        _chars = 1 + L.length s
        _words = fromIntegral $! length $! L.words s

runner :: [String] -> IO [(Counts, FilePath)]
-- run the totals function for each input, report totals together for
-- pretty presentation
runner args
        | null files = do
            counts <- totals <$> L.getContents
            return [(counts, "")]

        | otherwise  = mapM wc files
    where
        files = filter (`notElem` ["-", "--"]) args

        wc path = do
            counts <- totals <$> L.readFile path
            return (counts, path)

display :: Options -> [(Counts, FilePath)] -> IO ()
-- produce a total if necessary
display o xs
        | length xs > 1 = pretty o $ xs ++ [(total, "total")]
        | otherwise     = pretty o xs
    where
        total = foldl' summation (0, 0, 0) $ map fst xs

pretty :: Options -> [(Counts, FilePath)] -> IO ()
-- padding aware columnar display for results
pretty (Options ow ol oc) xs =
        mapM_ putStrLn $
        reverse $
        map unwords $
        rotate
            [ smartBuffer Righty l
            , smartBuffer Righty w
            , smartBuffer Righty c
            , smartBuffer Lefty  p
            ]
    where
        l = if ol then map ((\(a, _, _) -> show a) . fst) xs else []
        w = if ow then map ((\(_, a, _) -> show a) . fst) xs else []
        c = if oc then map ((\(_, _, a) -> show a) . fst) xs else []
        p = map snd xs

wcMain :: [String] -> IO ()
-- parse arguments, do the work
wcMain args = do
        let (actions, files, errors) = getOpt RequireOrder options args

        unless (null errors) $ do
            mapM_ putStr errors
            exitFailure

        case foldM (flip id) defaults actions of
            Left   err     -> die err

            Right (Options False False False) ->
                runner files >>= display (Options True True True)

            Right opts -> runner files >>= display opts

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
            (\opt -> Right opt { optLines = True }))
        "print the word count"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "wc" options))
        "show this help text"
    ]

-- utility

rotate :: [[a]] -> [[a]]
rotate = reverse . transpose

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
