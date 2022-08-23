module Main where

import qualified Data.HashMap.Strict as H
import           Data.List
import           System.Environment
import           System.Exit

import           Coreutils.Awk       (Awk (..))
import           Coreutils.Basename  (Basename (..))
import           Coreutils.Cat       (Cat (..))
import           Coreutils.Cut       (Cut (..))
import           Coreutils.Dirname   (Dirname (..))
import           Coreutils.Echo      (Echo (..))
import           Coreutils.Env       (Env (..))
import           Coreutils.Head      (Head (..))
import           Coreutils.Mkdir     (Mkdir (..))
import           Coreutils.Nl        (Nl (..))
import           Coreutils.Nologin   (Nologin (..))
import           Coreutils.Pwd       (Pwd (..))
import           Coreutils.Random    (Random (..))
import           Coreutils.Rev       (Rev (..))
import           Coreutils.Seq       (Seq (..))
import           Coreutils.Sleep     (Sleep (..))
import           Coreutils.Split     (Split (..))
import           Coreutils.Sponge    (Sponge (..))
import           Coreutils.Sum       (Sum (..))
import           Coreutils.Tac       (Tac (..))
import           Coreutils.Tee       (Tee (..))
import           Coreutils.Test      (Test (..))
import           Coreutils.Tr        (Tr (..))
import           Coreutils.Uniq      (Uniq (..))
import           Coreutils.Wc        (Wc (..))
import           Coreutils.Which     (Which (..))
import           Coreutils.Whoami    (Whoami (..))
import           Coreutils.Yes       (Yes (..))

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
dispatch (cmd:args) =
    H.lookupDefault (const $ die usage) cmd table args
dispatch _ = die usage

usage :: String
usage = unlines $ ["", "usage:"] <> commands
    where
        commands = map ("  " <>) $ sort $ H.keys table

table :: H.HashMap String ([String] -> IO ())
table = H.fromList
    [ ("awk", run Awk)
    , ("basename", run Basename)
    , ("cat", run Cat)
    , ("cut", run Cut)
    , ("dirname", run Dirname)
    , ("echo", run Echo)
    , ("env", run Env)
    , ("head", run Head)
    , ("mkdir", run Mkdir)
    , ("nl", run Nl)
    , ("nologin", run Nologin)
    , ("pwd", run Pwd)
    , ("random", run Random)
    , ("rev", run Rev)
    , ("seq", run Seq)
    , ("sleep", run Sleep)
    , ("split", run Split)
    , ("sponge", run Sponge)
    , ("sum", run Sum)
    , ("tac", run Tac)
    , ("tee", run Tee)
    , ("test", run Test)
    , ("tr", run Tr)
    , ("uniq", run Uniq)
    , ("wc", run Wc)
    , ("which", run Which)
    , ("whoami", run Whoami)
    , ("yes", run Yes)
    ]
