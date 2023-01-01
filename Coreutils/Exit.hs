module Coreutils.Exit where

import           Coreutils.Util
import           Data.Char
import           System.Exit

data Exit = Exit
data Utrue = Utrue
data Ufalse = Ufalse

instance Util Exit where
    run _ = exitMain

exitMain :: [String] -> IO ()
exitMain [] = exitSuccess
exitMain [c]
        | number && code == 0 = exitSuccess
        | number = exitWith (ExitFailure code)
        | otherwise = die $ c <> " is not a valid numeric exit code"
    where
        number = all isDigit c
        code = min 255 (read c)
exitMain _ = die "too many arguments"

instance Util Utrue where
    run _ _ = exitSuccess

instance Util Ufalse where
    run _ _ = exitFailure
