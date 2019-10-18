{-# LANGUAGE OverloadedStrings #-}

module Coreutils.Head where

-- head
--
-- show some number of lines or characters from the top or bottom of files
-- or stdin. if both -c and -l are provided, -c is preferred
--
-- lazy implementation allows things like `head -l 5 /dev/urandom`
-- negative options `-l -10`, `-c -100` require the entire file to fit in memory


import           Control.Monad
import           Data.ByteString.Lazy  (ByteString)
import qualified Data.ByteString.Lazy  as L
import           GHC.Int               (Int64)
import           System.Console.GetOpt
import           System.Exit

import           Coreutils.Util

type NumLines = Int
type NumChars = Int64

data Options = Options
        { optQuiet :: Bool
        , optLines :: NumLines
        , optChars :: Maybe NumChars
        }

defaults :: Options
defaults = Options
        { optQuiet = False
        , optLines = 10
        , optChars = Nothing
        }

options :: [OptDescr (Options -> Either String Options)]
options =
    [ Option "n" ["lines"]
        (ReqArg
            (\arg opt -> case reads arg of
              [(n, "")] -> Right opt { optLines = n }
              _         -> Left $ "error: '" <> arg <> "' is not a number")
            "LINES")
        "Number of lines"

    , Option "c" ["bytes"]
        (ReqArg
            (\arg opt -> case reads arg of
              [(n, "")] -> Right opt { optChars = Just n }
              _         -> Left $ "error: '" <> arg <> "' is not a number")
            "LINES")
        "Number of characters"

    , Option "q" ["quiet", "silent"]
        (NoArg
            (\opt -> Right opt { optQuiet = True }))
        "Do not show headers for files"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "head" options))
        "Show this help text"
    ]


charsOp :: NumChars -> ByteString -> ByteString
-- ^ take or drop the required number of characters
charsOp n content
        | n >= 0    = L.take n content
        | otherwise = L.drop (L.length content - negate n) content


linesOp :: NumLines -> ByteString -> ByteString
-- ^ use lazy newline counting to determine how much of the file to process
linesOp n content
        | n == 0    = ""
        | n > 0     = case indices of
                          [] -> content
                          _  -> L.take (last indices + 1) content
        | otherwise = case secidni of
                          [] -> content
                          _  -> L.drop (last secidni + 1) content
    where
        secidni = take (abs n + 1) $ reverse positions
        indices = take n positions
        newline = 10
        positions = L.elemIndices newline content


charHead :: NumChars -> FilePath -> IO ()
charHead n f = charsOp n <$> L.readFile f >>= L.putStr

lineHead :: NumLines -> FilePath -> IO ()
lineHead n f = linesOp n <$> L.readFile f >>= L.putStr


header :: (FilePath -> IO ()) -> FilePath -> IO ()
-- ^ header function used when multiples files are processed together
header f s = do
    putStrLn $ "==> " <> s <> " <=="
    f s
    putStrLn ""


type HeaderFunction = (FilePath -> IO ()) -> FilePath -> IO ()

switch :: HeaderFunction -> NumLines -> Maybe NumChars -> [FilePath] -> IO ()
-- ^ switchboard for business logic
-- no input files? use stdin
-- multiple files? use the header function
switch _ _ (Just n) []  = L.interact $ charsOp n
switch _ n _        []  = L.interact $ linesOp n

switch _ _ (Just n) [f] = charHead n f
switch _ n _        [f] = lineHead n f

switch h _ (Just n) fs  = mapM_ (h (charHead n)) fs
switch h n _        fs  = mapM_ (h (lineHead n)) fs


data Head = Head

instance Util Head where
    run _ args = do
        let (actions, files, errors) = getOpt RequireOrder options args

        unless (null errors) $ do
            mapM_ putStr errors
            exitFailure

        case foldM (flip id) defaults actions of
            Left   err -> die err
            Right opts -> do
                let Options { optChars = nChars
                            , optLines = nLines
                            , optQuiet = quiet
                            } = opts
                    hFunc = if quiet then id else header

                switch hFunc nLines nChars files
