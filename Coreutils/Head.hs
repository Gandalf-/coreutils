{-# LANGUAGE OverloadedStrings #-}

module Coreutils.Head where

-- head
--
-- show some number of lines or characters from the top or bottom of files
-- or stdin.
--
-- this has constant memory usage even with negative values on streams

import           Control.Monad
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as C
import           Data.Char
import           Data.Int
import           Streaming
import qualified Streaming.ByteString.Char8 as Q
import qualified Streaming.Prelude          as S
import           System.Console.GetOpt
import           System.Exit
import           System.IO

import           Coreutils.Util

data Head = Head

instance Util Head where
    run _ = headMain

-- | IO

headMain :: [String] -> IO ()
headMain args = do
        unless (null errors) $
            die $ unlines errors
        either die (`runHead` other) $
            foldM (flip id) defaultOptions opts
    where
        (opts, other, errors) = getOpt RequireOrder optionDesc args

runHead :: Options -> [String] -> IO ()
runHead os []  = runHead os ["-"]
runHead os [f] = single os { optQuiet = True } f
runHead os fs
        | optQuiet os = mapM_ (single os) fs
        | otherwise   = multiple fs
    where
        multiple :: [FilePath] -> IO ()
        multiple []     = pure ()
        multiple [x]    = single (os { optLast = True }) x
        multiple (x:xs) = single os x >> multiple xs

single :: Options -> FilePath -> IO ()
single os f = case f of
        "-" -> runner Q.stdin
        _   -> withFile f ReadMode $ runner . Q.fromHandle
    where
        runner :: Q.ByteStream IO () -> IO ()
        runner = Q.stdout . header (getRuntime os f)

-- | Implementation

data Action = HeadBytes Int64 | HeadLines Int

execute :: MonadIO m => Action -> Q.ByteStream m () -> Q.ByteStream m ()
execute (HeadBytes n) = Q.take n
execute (HeadLines n) = Q.unlines . S.take n . Q.lines

data Runtime = Runtime {
      action  :: Action
    , prefix  :: ByteString
    , postfix :: ByteString
}

header :: MonadIO m => Runtime -> Q.ByteStream m () -> Q.ByteStream m ()
header rt bs = do
    Q.fromStrict $ prefix rt
    execute (action rt) bs
    Q.fromStrict $ postfix rt

getRuntime :: Options -> FilePath -> Runtime
getRuntime os f =
        Runtime (optAction os) prefix' postfix'
    where
        prefix'
            | optQuiet os = C.empty
            | otherwise   = C.concat ["==> ", C.pack f, " <==\n"]
        postfix'
            | optQuiet os      = C.empty
            | not (optLast os) = "\n"
            | otherwise        = C.empty

-- | Options

data Options = Options {
      optAction :: Action
    , optQuiet  :: Bool
    , optLast   :: Bool
    }

defaultOptions :: Options
defaultOptions = Options {
      optAction = HeadLines 10
    , optQuiet = False
    , optLast = False
    }

parseNumber :: Read a => String -> Either String a
parseNumber xs
    | null xs              = Left "no number provided"
    | not (all isDigit xs) = Left $ xs <> " is not a number"
    | otherwise            = Right $ read xs

parseBytes :: String -> Either String Int64
parseBytes xs =
        (*) <$> parseNumber digits <*> get suffix
    where
        (digits, suffix) = span isDigit xs
        get :: String -> Either String Int64
        get []      = pure 1
        get "b"     = pure 512
        get [p]     = (1024 ^) <$> power p
        get (p:"B") = (1000 ^) <$> power p
        get bs      = Left $ bs <> " is not a recognized byte suffix"

power :: Char -> Either String Int64
power 'k' = pure 1
power p = case C.elemIndex p " KMGTPEY" of
    Just i  -> pure $ fromIntegral i
    Nothing -> Left $ p : " is not a recognized byte suffix"

optionDesc :: [OptDescr (Options -> Either String Options)]
optionDesc =
    [ Option "n" ["lines"]
        (ReqArg
            (\arg opt -> do
                n <- parseNumber arg
                Right opt { optAction = HeadLines n }
            )
            "LINES")
        "Number of lines"

    , Option "c" ["bytes"]
        (ReqArg
            (\arg opt -> do
                n <- parseBytes arg
                Right opt { optAction = HeadBytes n }
            )
            "CHARS")
        "Number of characters, may include a suffix like kB, M, MB, G, GB, etc"

    , Option "q" ["quiet", "silent"]
        (NoArg
            (\opt -> Right opt { optQuiet = True }))
        "Do not show headers for files"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "head" optionDesc))
        "Show this help text"
    ]
