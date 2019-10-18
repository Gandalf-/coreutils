module Coreutils.Nologin where

-- nologin
--
-- exit failure with a message

import           Coreutils.Util
import           System.Exit    (die)

data Nologin = Nologin

instance Util Nologin where
    run _ _ = die "This account is currently not available."
