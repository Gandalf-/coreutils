module Coreutils.Tee where

import           Control.Monad
import qualified Data.ByteString.Streaming as Q
import           System.Console.GetOpt
import           System.Exit
import           System.IO

import           Coreutils.Util

data Tee = Tee

instance Util Tee where
    run _ = teeMain

newtype Options = Options { optMode :: IOMode }

teeMain :: [String] -> IO ()
teeMain args = do
        let (actions, filenames, errors) = getOpt RequireOrder options args

        unless (null errors) $ do
            mapM_ putStr errors
            exitFailure

        case foldM (flip id) defaults actions of
            Left   err -> die err
            Right opts -> runTee opts filenames


runTee :: Options -> [FilePath] -> IO ()
-- get handles for all output files and stdout, run them through tee
runTee o fs = do
        handles <- mapM (`openBinaryFile` optMode o) fs
        tee handles
        mapM_ hClose handles


tee :: [Handle] -> IO ()
-- build up n computations that copy the stream, then write it a file
tee = Q.stdout . foldr (\h -> Q.toHandle h . Q.copy) Q.stdin


-- | Options

defaults :: Options
defaults = Options { optMode = WriteMode }

options :: [OptDescr (Options -> Either String Options)]
options =
    [ Option "a" ["append"]
        (NoArg
            (\opt -> Right opt { optMode = AppendMode }))
        "append to given files, do not overwrite"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "head" options))
        "Show this help text"
    ]
