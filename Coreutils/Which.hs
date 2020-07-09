module Coreutils.Which where

-- which
--
-- look for the arguments on $PATH

import           Control.Monad
import           Data.Maybe
import           System.Directory
import           System.Exit

import           Coreutils.Util

data Which = Which

instance Util Which where
    run _ = which


which :: [String] -> IO ()
which args = do
        paths <- mapM find names
        mapM_ (putStr . unlines) paths
        when (any null paths) exitFailure
    where
        (find, names) = case args of
            ("-a":xs) -> (findExecutables, xs)
            _         -> (findSingle, args)

        findSingle n = maybeToList <$> findExecutable n
