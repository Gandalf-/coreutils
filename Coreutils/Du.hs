module Control.Du where

import           Control.Exception
import           Control.Monad
import           System.Console.GetOpt
import           System.Exit
import           System.IO

import           Coreutils.Util

data Du = Du

instance Util Du where
    run _ = duMain

newtype TreeStat = TreeStat {
        _bytes :: Integer
    }

data Options = Options {
        optTotal  :: Bool,
        optHuman  :: Bool,
        optFollow :: Bool,
        optPowers :: Integer
    }

duMain :: [String] -> IO ()
duMain args = do
        unless (null errors) $
            die $ unlines errors

        either die (`runDu` filenames) $
            foldM (flip id) defaults opts
    where
        (opts, filenames, errors) = getOpt RequireOrder options args

runDu :: Options -> [FilePath] -> IO ()
runDu = undefined

du :: FilePath -> IO TreeStat
du = undefined

-- | Options

defaults :: Options
defaults = Options {
        optTotal = False,
        optHuman = False,
        optFollow = False,
        optPowers = 1024
    }

options :: [OptDescr (Options -> Either String Options)]
options =
    [ Option "c" ["total"]
        (NoArg
            (\opt -> Right opt { optTotal = True }))
        "produce a grand total"

    , Option "h" ["human-readable"]
        (NoArg
            (\opt -> Right opt { optHuman = True }))
        "print sizes in human readable format"

    , Option "L" ["dereference"]
        (NoArg
            (\opt -> Right opt { optFollow = True }))
        "dereference all symbolic links"

    , Option "P" ["no-dereference"]
        (NoArg
            (\opt -> Right opt { optFollow = False }))
        "do not dereference any symbolic links"

    , Option "" ["si"]
        (NoArg
            (\opt -> Right opt { optPowers = 1000, optHuman = True }))
        "human readable, but using powers of 1000 not 1024"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "du" options))
        "show this help text"
    ]
