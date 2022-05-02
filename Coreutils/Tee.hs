module Coreutils.Tee where

import           Control.Exception
import           Control.Monad
import qualified Streaming.ByteString      as Q
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
        unless (null errors) $
            die $ unlines errors

        either die (`runTee` filenames) $
            foldM (flip id) defaults opts
    where
        (opts, filenames, errors) = getOpt RequireOrder options args


runTee :: Options -> [FilePath] -> IO ()
-- get handles for all output files and stdout, run them through tee
runTee o fs =
        bracket acquire release tee
    where
        acquire = mapM (`openBinaryFile` optMode o) fs
        release = mapM_ hClose


tee :: [Handle] -> IO ()
-- build up n computations that copy the stream and write it a file
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
            (\_ -> Left $ usageInfo "tee" options))
        "show this help text"
    ]
