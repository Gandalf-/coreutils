module Coreutils.Tee where

import           Control.Monad
import qualified Data.ByteString as B
import           System.Console.GetOpt
import           System.IO
import           System.Exit

import           Coreutils.Util

data Tee = Tee

instance Util Tee where
    run _ = teeMain

data Options = Options
        { optAppend   :: Bool
        }

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
runTee o fs = do
        files <- mapM (flip openBinaryFile mode) fs
        let handles = stdout : files
        tee handles
        mapM_ hClose handles
    where
        mode = if optAppend o then AppendMode else WriteMode


tee :: [Handle] -> IO ()
tee hs = do
        done <- isEOF
        unless done $ do
            buffer <- B.hGetSome stdin bufferSize
            mapM_ (flip B.hPut buffer) hs
            tee hs
    where
        bufferSize = 1024 * 32 -- 32K

-- | Options

defaults :: Options
defaults = Options
        { optAppend = False
        }

options :: [OptDescr (Options -> Either String Options)]
options =
    [ Option "a" ["append"]
        (NoArg
            (\opt -> Right opt { optAppend = True }))
        "append to given files, do not overwrite"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "head" options))
        "Show this help text"
    ]
