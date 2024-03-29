module Coreutils.Rev where

-- rev
--
-- read lines from stdin or files, print them out with content reversed

import qualified Data.ByteString.Char8      as C
import           Streaming
import qualified Streaming.ByteString.Char8 as Q
import qualified Streaming.Prelude          as S
import           System.IO

import           Coreutils.Util

data Rev = Rev

instance Util Rev where
    run _ = revMain

revMain :: [String] -> IO ()
-- typical stuff, determine stdin versus some number of files
revMain args
        | null args = switch "-"
        | otherwise = mapM_ switch args
    where
        switch "-"  = Q.interact rev
        switch path = withFile path ReadMode fileRev

        fileRev :: Handle -> IO ()
        fileRev = Q.stdout . rev . Q.fromHandle

rev :: MonadIO m => Q.ByteStream m () -> Q.ByteStream m ()
-- reverse each line in the file
rev = Q.unlines
    . S.subst Q.chunk
    . S.map C.reverse
    . mapped Q.toStrict
    . Q.lines
