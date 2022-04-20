module Coreutils.Dirname where

import           Coreutils.Util

data Dirname = Dirname

instance Util Dirname where
    run _ = mapM_ (putStrLn . dirname)

dirname :: String -> String
dirname name
        | empty && hasRoot = "/"
        | empty            = "."
        | otherwise        = reduced
    where
        empty   = null reduced
        reduced = reduce name
        hasRoot = case name of
            ('/':_) -> True
            _       -> False

reduce :: String -> String
reduce =
    reverse
    . dropWhile (== '/')
    . dropWhile (/= '/')
    . dropWhile (== '/')
    . reverse
