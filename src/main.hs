module Main where

import           Data.List          (isPrefixOf)
import           System.Environment (getArgs, getProgName)
import           System.Exit        (die)

import           Coreutils.Cat
import           Coreutils.Cut
import           Coreutils.Echo
import           Coreutils.Head
import           Coreutils.Nologin
import           Coreutils.Pwd
import           Coreutils.Rev
import           Coreutils.Seq
import           Coreutils.Sleep
import           Coreutils.Tr
import           Coreutils.Uniq
import           Coreutils.Wc
import           Coreutils.Which
import           Coreutils.Yes

import           Coreutils.Util


main :: IO ()
main = getProgName >>= choose

choose :: String -> IO ()
choose name
    | "utils" `isPrefixOf` name =
        getArgs >>= dispatch

    | otherwise = do
        args <- getArgs
        dispatch $ name : args

dispatch :: [String] -> IO ()
dispatch ("cat":xs)     = run Cat xs
dispatch ("cut":xs)     = run Cut xs
dispatch ("echo":xs)    = run Echo xs
dispatch ("head":xs)    = run Head xs
dispatch ("nologin":xs) = run Nologin xs
dispatch ("pwd":xs)     = run Pwd xs
dispatch ("rev":xs)     = run Rev xs
dispatch ("seq":xs)     = run Seq xs
dispatch ("sleep":xs)   = run Sleep xs
dispatch ("tr":xs)      = run Tr xs
dispatch ("uniq":xs)    = run Uniq xs
dispatch ("wc":xs)      = run Wc xs
dispatch ("which":xs)   = run Which xs
dispatch ("yes":xs)     = run Yes xs
dispatch _              = die "usage: ..."
