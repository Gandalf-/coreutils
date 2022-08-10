module Coreutils.Whoami where

-- Read the environment to determine the current user's name

import           Coreutils.Util
import           System.Environment
import           System.Info

data Whoami = Whoami

instance Util Whoami where
    run _ _ = getEnv who >>= putStrLn

who :: String
who
    | os == "mingw32" = "USERNAME"
    | otherwise       = "USER"
