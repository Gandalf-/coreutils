module Coreutils.Basename where

-- basename
--
-- Return the non-directory portion of a pathname

import Data.List.Split
import System.Exit

import Coreutils.Util

data Basename = Basename

instance Util Basename where
    run _  = either die putStrLn . runner

type Error = String

runner :: [String] -> Either Error FilePath
runner [p]    = pure $ basename p
runner [p, f] = pure $ suffix f $ basename p
runner _      = Left "path [suffix]"

basename :: FilePath -> FilePath
basename [] = []
basename xs =
        last . orSlash . filter (not . null) $ splitOn "/" xs
    where
        orSlash [] = ["/"]
        orSlash os = os

suffix :: FilePath -> String -> FilePath
suffix _ [] = []
suffix suf (x:xs)
    | xs == suf = [x]
    | otherwise = x : suffix suf xs
