module Coreutils.Realpath where

import           Control.Exception
import           Coreutils.Util
import           System.Directory
import           System.Exit

data Realpath = Realpath

instance Util Realpath where
    run _ = rpMain

-- | IO

rpMain :: [String] -> IO ()
rpMain args =
        go args >>= mapM_ (either die putStrLn)
    where
        go ("-q":fs) = runner quiet fs
        go fs        = runner verbose fs

        verbose = Runtime getCurrentDirectory canonicalizePath show
        quiet   = verbose { onError = const ""}

-- | Implementation

data Runtime = Runtime {
    defaultPath :: IO FilePath,
    realpath    :: FilePath -> IO FilePath,
    onError     :: IOException -> String
}

type Result = Either String FilePath

runner :: Runtime -> [FilePath] -> IO [Result]
runner rt [] = (: []) <$> (defaultPath rt >>= execute rt)
runner rt fs = mapM (execute rt) fs

execute :: Runtime -> FilePath -> IO Result
execute rt target =
        process <$> try (realpath rt target)
    where
        process (Left err)   = Left $ onError rt err
        process (Right path) = Right path
