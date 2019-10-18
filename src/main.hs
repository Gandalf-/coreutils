{-# LANGUAGE LambdaCase #-}

module Main where

import           System.Environment (getArgs)
import           System.Exit        (die)

import           Coreutils.Cat
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
main = getArgs >>= \case
    ("cat":xs)     -> run Cat xs
    ("echo":xs)    -> run Echo xs
    ("head":xs)    -> run Head xs
    ("nologin":xs) -> run Nologin xs
    ("pwd":xs)     -> run Pwd xs
    ("rev":xs)     -> run Rev xs
    ("seq":xs)     -> run Seq xs
    ("sleep":xs)   -> run Sleep xs
    ("tr":xs)      -> run Tr xs
    ("uniq":xs)    -> run Uniq xs
    ("wc":xs)      -> run Wc xs
    ("which":xs)   -> run Which xs
    ("yes":xs)     -> run Yes xs
    _              -> die "usage: ..."
