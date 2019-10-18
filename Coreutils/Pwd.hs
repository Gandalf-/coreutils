module Coreutils.Pwd where

-- pwd
--
-- output the current working directory

import           Coreutils.Util
import           System.Directory (getCurrentDirectory)

data Pwd = Pwd

instance Util Pwd where
    run _ _ = getCurrentDirectory >>= putStrLn
