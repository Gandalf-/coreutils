module Coreutils.Rev where

-- rev
--
-- read lines from stdin, print them out reversed

import           Coreutils.Util

data Rev = Rev

instance Util Rev where
    run _ _ = interact rev
        where
            rev = unlines . map reverse . lines
