module Coreutils.Rev where

-- rev
--
-- read lines from stdin or files, print them out with content reversed

import           Coreutils.Util

data Rev = Rev

instance Util Rev where
    run _ = runRev


runRev :: [String] -> IO ()
-- invoke rev on some number of input files. no files means stdin
runRev [] = interact rev
runRev fs = mapM_ (\ filename -> rev <$> readFile filename >>= putStr) fs

rev :: String -> String
rev = unlines . map reverse . lines
