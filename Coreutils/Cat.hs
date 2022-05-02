module Coreutils.Cat where

-- cat
--
-- read files from the command line or echo stdin

import           Control.Exception
import           Control.Monad
import qualified Streaming.ByteString      as Q
import           System.Exit
import           System.Info
import           System.IO

import           Coreutils.Util

data Cat = Cat

instance Util Cat where
    run _ = runner

runner :: [String] -> IO ()
-- attempt to write each input file to stdout, continues on intermediary failures
runner args
        | null files = piper cat
        | otherwise  = do
            failures <- mapM switch files
            when (or failures) exitFailure
    where
        switch "-" = wrap (piper cat)
        switch fn  = wrap (withFile fn ReadMode cat)

        cat = Q.stdout . Q.fromHandle

        files = filter (/= "--") args
        wrap f = catch (f >> pure False) (\e -> pError e >> pure True)

        pError :: SomeException -> IO ()
        pError = hPrint stderr

piper :: (Handle -> IO ()) -> IO ()
-- this allows us to read from stdin multiple times; used by some build scripts
piper f
    | os == "mingw32" = f stdin
    | otherwise       = withFile "/dev/tty" ReadMode f
