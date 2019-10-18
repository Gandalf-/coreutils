module Coreutils.Util where

class Util a where
    run :: a -> [String] -> IO ()
