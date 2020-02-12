module Coreutils.Rev where

-- rev
--
-- read lines from stdin or files, print them out with content reversed

import           Coreutils.Util
import qualified Data.ByteString.Lazy.Char8 as L

data Rev = Rev

instance Util Rev where
    run _ = runRev

runRev :: [String] -> IO ()
-- invoke rev on some number of input files. no files means stdin
runRev [] = L.interact rev
runRev fs = mapM_ (\ filename -> rev <$> L.readFile filename >>= L.putStr) fs

rev :: L.ByteString -> L.ByteString
rev = L.unlines . map L.reverse . L.lines
