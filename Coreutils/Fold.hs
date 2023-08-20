module Coreutils.Fold where

import           Coreutils.Util
import           Data.Char
import           System.Console.GetOpt

data Fold = Fold

instance Util Fold where
    run _ _ = undefined


folder :: Options -> String -> String
folder os xs =
        go 0 (optWidth os) $ enumerate os xs

enumerate :: Options -> String -> [(Int, Char)]
enumerate os xs =
        zip pos xs
    where
        pos = map (positions (optPosition os)) xs

go :: Int -> Int -> [(Int, Char)] -> String
go _ _ [] = ""
go index len xs@((p, c):ixs)
    | c == '\n'       = c : go 0 len ixs
    | p + index > len = '\n' : go (index - len) len xs
    | otherwise       = c : go (index + p) len ixs

data Position = Bytes | Columns

positions :: Position -> Char -> Int
positions Bytes _      = 1
positions Columns '\t' = 8
positions Columns '\r' = -1
positions Columns '\b' = -1
positions Columns _    = 1

-- | Options

data Options = Options
    { optWidth    :: Int
    , optPretty   :: Bool
    , optPosition :: Position
    }

defaultOptions :: Options
defaultOptions = Options
    { optWidth = 80
    , optPretty = False
    , optPosition = Columns
    }

parseWidth :: String -> Either String Int
parseWidth xs
    | all isDigit xs = Right $ read xs
    | otherwise = Left $ xs <> " is not a valid width"

options :: [OptDescr (Options -> Either String Options)]
options =
    [ Option "b" []
        (NoArg
            (\opt -> Right opt { optPosition = Bytes }))
        "Count width in bytes intead of column positions"

    , Option "s" []
        (NoArg
            (\opt -> Right opt { optPretty = True }))
        "Break lines on spaces when possible"

    , Option "b" []
        (ReqArg
            (\arg opt -> (\w -> opt { optWidth = w }) <$> parseWidth arg)
            "width")
        "Line width to use instead of the default of 80"
    ]
