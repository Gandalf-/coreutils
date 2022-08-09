module Coreutils.Seq where

-- seq
--
-- produce a sequence of numbers with eccentric formatting behavior

import           Control.Monad
import           Data.Char
import           Data.Maybe
import           System.Console.GetOpt
import           System.Exit
import           Text.Printf           (printf)

import           Coreutils.Util

data Seq = Seq

instance Util Seq where
    run _ = seqMain

-- | IO

seqMain :: [String] -> IO ()
seqMain args = do
        unless (null errors) $
            die $ unlines errors

        either die (`runner` arguments) $
            foldM (flip id) defaultOptions opts
    where
        (opts, arguments, errors) = getOpt RequireOrder options args

runner :: Options -> [String] -> IO ()
runner os ss = either die exe (getRuntime os ss)
    where
        exe rt = mapM_ (putStr . format rt) (values rt)

-- | Implementation

type Value = Double

data Runtime = Runtime {
      format :: Value -> String
    , values :: [Value]
}

getRuntime :: Options -> [String] -> Either String Runtime
getRuntime os ss = do
    (bs, f) <- getBounds ss
    let fmt = fromMaybe (show f) (optFormat os) <> optSeparator os
    pure $ Runtime {
          format = printf fmt
        , values = expand bs
    }

data Bounds = Bounds {
      _start :: Double
    , _inc   :: Double
    , _end   :: Double
}
    deriving (Eq, Show)

expand :: Bounds -> [Value]
expand (Bounds s i e) =
        filter valid [s, s + i .. e]
    where
        valid = if i > 0 then (<= e) else (>= e)

data Format = IntFormat | DecFormat Int
    deriving (Eq)

instance Ord Format where
    compare IntFormat      IntFormat    = EQ
    compare IntFormat     (DecFormat _) = LT
    compare (DecFormat _) IntFormat     = GT
    compare (DecFormat a) (DecFormat b) = compare a b

instance Show Format where
    show IntFormat     = "%.0f"
    show (DecFormat w) = "%." <> show w <> "g"

getBounds :: [String] -> Either String (Bounds, Format)
getBounds [l] = do
    (e, _) <- parse l
    pure (Bounds 1 1 e, IntFormat)

getBounds [f, l] = do
    (start, sf) <- parse f
    (end,   _ ) <- parse l
    pure (Bounds start 1 end, sf)

getBounds [f, i, l] = do
    (start, sf) <- parse f
    (inc,   cf) <- parse i
    (end,   _ ) <- parse l
    when (inc == 0) $ Left "the increment may not be zero"
    pure (Bounds start inc end, maximum [sf, cf])

getBounds [] = Left "too few arguments"
getBounds _  = Left "too many arguments"

parse :: String -> Either String (Double, Format)
parse ('-':xs) = do
    (v, f) <- parse xs
    pure (negate v, f)
parse ('.':xs) =
    parse $ "0." <> xs
parse s
    | integerish s = Right (read s, IntFormat)
    | doubleish s  = Right (read s, DecFormat width)
    | otherwise    = Left $ s <> " is not a number"
    where
        integerish = all isDigit
        doubleish  = all (\c -> isDigit c || c == '.')
        width      = length $ tail $ dropWhile (/= '.') s

-- | Options

data Options = Options {
      optFormat    :: Maybe String
    , optSeparator :: String
    }

defaultOptions :: Options
defaultOptions = Options {
      optFormat     = Nothing
    , optSeparator  = "\n"
    }

options :: [OptDescr (Options -> Either String Options)]
options =
    [ Option "f" ["format"]
        (ReqArg
            (\arg opt -> Right opt { optFormat = Just arg })
            "FORMAT")
        "provide the printf format to use while printing"

    , Option "s" ["separator"]
        (ReqArg
            (\arg opt -> Right opt { optSeparator = arg})
            "SEPARATOR")
        "use this string between numbers"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "head" options))
        "Show this help text"
    ]
