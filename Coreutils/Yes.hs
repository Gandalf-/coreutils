module Coreutils.Yes where

import           Coreutils.Util

-- yes
--
-- repeat the command line arguments to stdout

data Yes = Yes

instance Util Yes where
    run _ args = yes $ concat args

yes :: String -> IO ()
yes [] = putStrLn "y" >> yes []
yes xs = putStrLn xs  >> yes xs
