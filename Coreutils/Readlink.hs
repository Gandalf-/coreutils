module Coreutils.Readlink where

import           Control.Monad
import           System.Console.GetOpt
import           System.Directory
import           System.Exit

import           Coreutils.Util

data Readlink = Readlink

instance Util Readlink where
    run _ = readlinkMain

readlinkMain :: [String] -> IO ()
readlinkMain args = do
        unless (null errors) $
            die $ unlines errors
        either die (`runReadlink` other) $
            foldM (flip id) defaultRuntime opts
    where
        (opts, other, errors) = getOpt RequireOrder optionDesc args

runReadlink :: Runtime -> [String] -> IO ()
runReadlink o = mapM_ (execute o >=> display o)

data Runtime = Runtime
    { display :: FilePath -> IO ()
    , execute :: FilePath -> IO FilePath
    }

defaultRuntime :: Runtime
defaultRuntime = Runtime
    { display = putStrLn
    , execute = getSymbolicLinkTarget
    }

optionDesc :: [OptDescr (Runtime -> Either String Runtime)]
optionDesc =
    [ Option "f" []
        (NoArg
            (\opt -> Right opt { execute = canonicalizePath }))
        "canonicalize to a full path"

    , Option "n" []
        (NoArg
            (\opt -> Right opt { display = putStr }))
        "do not output a trailing newline"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "readlink [-fn] [FILE...]" optionDesc))
        "show this help text"
    ]
