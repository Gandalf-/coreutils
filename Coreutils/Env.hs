module Coreutils.Env where

-- env
--
-- run a command in a modified environment

import           Control.Monad
import           Data.List
import           System.Console.GetOpt
import           System.Directory
import           System.Environment
import           System.Exit
import           System.Process

import           Coreutils.Util

data Env = Env

instance Util Env where
    run _ = envMain

data Options = Options {
          optEmpty   :: Bool
        , optWorking :: Maybe FilePath
        , optClear   :: [String]
    }
    deriving (Show, Eq)

data Runtime = Runtime {
          variables :: [(String, String)]
        , command   :: Maybe (String, [String])
        , options   :: Options
    }
    deriving (Show, Eq)

envMain :: [String] -> IO ()
envMain args = do
        unless (null errors) $
            die $ unlines errors

        either die (`runEnv` arguments) $
            foldM (flip id) defaultOptions opts
    where
        (opts, arguments, errors) = getOpt RequireOrder optionDesc args

clearEnv :: IO ()
-- unset every variable defined
clearEnv = getEnvironment >>= mapM_ (unsetEnv . fst)

showEnv :: IO ()
-- print every variable defined
showEnv = sort <$> getEnvironment >>=
        mapM_ (\(k, v) -> putStrLn $ k <> "=" <> v)

runEnv :: Options -> [String] -> IO ()
-- clear environment, set working directory, set new variables, then run the
-- command or show the environment
runEnv os args = do
        when (optEmpty opts)
            clearEnv
        maybe
            (pure ())
            setCurrentDirectory
            $ optWorking opts
        mapM_ (uncurry setEnv) $ variables runtime
        maybe
            showEnv
            (uncurry callProcess)
            $ command runtime
    where
        runtime = getRuntime os args
        opts = options runtime

getRuntime :: Options -> [String] -> Runtime
-- apply all the parsers in order to create the runtime, 'what to do'
getRuntime opts a1 =
        uncurry argParser
        $ uncurry setParser
        $ optParser base a1
    where
        base = Runtime [] Nothing opts

optParser :: Runtime -> [String] -> (Runtime, [String])
-- consume - argument
optParser rt ("-":xs) = (rt { options = (options rt) { optEmpty = True}}, xs)
optParser rt xs       = (rt, xs)

setParser :: Runtime -> [String] -> (Runtime, [String])
-- consume key=value arguments
setParser rt (x:xs) =
        case envParser x of
            Nothing    -> (rt, x:xs)
            (Just set) -> setParser (rt { variables = variables rt <> [set]}) xs
setParser rt [] = (rt, [])

argParser :: Runtime -> [String] -> Runtime
-- consume command and arguments
argParser rt (x:xs) = rt { command = Just (x, xs)}
argParser rt []     = rt

-- | Options

defaultOptions :: Options
defaultOptions = Options {
          optEmpty = False
        , optWorking = Nothing
        , optClear = []
    }

optionDesc :: [OptDescr (Options -> Either String Options)]
optionDesc =
    [ Option "i" ["ignore-environment"]
        (NoArg
            (\opt -> Right opt { optEmpty = True }))
        "start with an empty environment"

    , Option "u" ["unset"]
        (ReqArg
            (\arg opt -> Right opt { optClear = arg : optClear opt })
            "NAME")
        "remove variable from the environment"

    , Option "C" ["chdir"]
        (ReqArg
            (\arg opt -> Right opt { optWorking = Just arg })
            "DIR")
        "change working directory to DIR"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "env" optionDesc))
        "show this help text"
    ]

-- Utility

envParser :: String -> Maybe (String, String)
-- key=value to (key, value), key= is valid too to unset
envParser s
        | null left = Nothing
        | not found = Nothing
        | otherwise = Just (left, right)
    where
        (left, right, found) = go s False ("", "")

        go ('=':xs) False o      = go xs True o
        go (x:xs)   False (l, r) = go xs False  (l <> [x], r)
        go (x:xs)   True  (l, r) = go xs True (l, r <> [x])
        go [] f (l, r)           = (l, r, f)
