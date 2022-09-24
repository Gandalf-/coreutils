module Main where

import           Coreutils.Util
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
import           Coreutils.Ls        (Ls (..))
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
    run (H.lookupDefault (Utility Usage) cmd table) args
dispatch xs = run Usage xs

data Usage = Usage
instance Util Usage where
    run _ _ = die $ unlines $ ["", "usage:"] <> commands
        where
            commands = map ("  " <>) $ sort $ H.keys table

table :: H.HashMap String Utility
table = H.fromList
    [ ("awk", Utility Awk)
    , ("basename", Utility Basename)
    , ("cat", Utility Cat)
    , ("cut", Utility Cut)
    , ("dirname", Utility Dirname)
    , ("echo", Utility Echo)
    , ("env", Utility Env)
    , ("head", Utility Head)
    , ("ls", Utility Ls)
    , ("mkdir", Utility Mkdir)
    , ("nl", Utility Nl)
    , ("nologin", Utility Nologin)
    , ("pwd", Utility Pwd)
    , ("random", Utility Random)
    , ("rev", Utility Rev)
    , ("seq", Utility Seq)
    , ("sleep", Utility Sleep)
    , ("split", Utility Split)
    , ("sponge", Utility Sponge)
    , ("sum", Utility Sum)
    , ("tac", Utility Tac)
    , ("tee", Utility Tee)
    , ("test", Utility Test)
    , ("tr", Utility Tr)
    , ("uniq", Utility Uniq)
    , ("wc", Utility Wc)
    , ("which", Utility Which)
    , ("whoami", Utility Whoami)
    , ("yes", Utility Yes)
    ]
