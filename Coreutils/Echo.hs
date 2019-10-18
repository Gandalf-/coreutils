module Coreutils.Echo where

-- echo
--
-- display something to the console
-- supports -n

import           Coreutils.Util

echo :: String -> IO ()
echo ('-' : 'n' : ' ' : xs) = putStr xs
echo xs                     = putStrLn xs

data Echo = Echo
instance Util Echo where
    run _ args = echo $ unwords args
