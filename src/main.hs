module Main where

import           Data.List          (isPrefixOf, sort)
import           System.Environment (getArgs, getProgName)
import           System.Exit        (die)

import           Coreutils.Cat      (Cat (..))
import           Coreutils.Cut      (Cut (..))
import           Coreutils.Echo     (Echo (..))
import           Coreutils.Head     (Head (..))
import           Coreutils.Nologin  (Nologin (..))
import           Coreutils.Pwd      (Pwd (..))
import           Coreutils.Rev      (Rev (..))
import           Coreutils.Seq      (Seq (..))
import           Coreutils.Sleep    (Sleep (..))
import           Coreutils.Split    (Split (..))
import           Coreutils.Tac      (Tac (..))
import           Coreutils.Tee      (Tee (..))
import           Coreutils.Tr       (Tr (..))
import           Coreutils.Uniq     (Uniq (..))
import           Coreutils.Wc       (Wc (..))
import           Coreutils.Which    (Which (..))
import           Coreutils.Yes      (Yes (..))

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
dispatch ("split":xs)   = run Split xs
dispatch ("tac":xs)     = run Tac xs
dispatch ("tee":xs)     = run Tee xs
dispatch ("tr":xs)      = run Tr xs
dispatch ("uniq":xs)    = run Uniq xs
dispatch ("wc":xs)      = run Wc xs
dispatch ("which":xs)   = run Which xs
dispatch ("yes":xs)     = run Yes xs
dispatch _              = die usage

usage :: String
usage = unlines $ ["", "usage:"] <> commands
    where
        commands = map (\c -> "  " <> c) $ sort
            [ "cat"
            , "cut"
            , "echo"
            , "head"
            , "nologin"
            , "pwd"
            , "rev"
            , "seq"
            , "sleep"
            , "split"
            , "tac"
            , "tee"
            , "tr"
            , "uniq"
            , "wc"
            , "which"
            , "yes"
            ]
