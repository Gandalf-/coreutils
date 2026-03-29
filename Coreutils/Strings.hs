module Coreutils.Strings where

import           Control.Monad              (foldM, (>=>))
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char
import           System.Console.GetOpt
import           System.Exit

import           Coreutils.Util

data Strings = Strings

instance Util Strings where
    run _ = stringsMain

-- | IO

stringsMain :: [String] -> IO ()
stringsMain args = do
    (opts, files) <- either die return $ parseArgs args
    let search = mapM_ B.putStrLn . strings (optLength opts)
    if null files
        then B.getContents >>= search
        else mapM_ (B.readFile >=> search) files

-- | Implementation

strings :: Int -> ByteString -> [ByteString]
strings len =
        filter valid . B.splitWith (not . printable)
    where
        valid bs = B.length bs >= fromIntegral len

printable :: Char -> Bool
printable c = valid
    where
        -- ASCII 32-126 only, matching C isprint()
        valid = value >= 32 && value <= 126
        value = ord c

-- | Options

newtype Options = Options { optLength :: Int }
    deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options 4

parseArgs :: [String] -> Either String (Options, [FilePath])
parseArgs args
    | null errors = foldM (flip id) defaultOptions opts >>= \o -> Right (o, other)
    | otherwise   = Left $ unlines errors
    where
        (opts, other, errors) = getOpt Permute optionDesc (expandArgs args)

-- | Expand bare-number shorthand: -NUM becomes -n NUM
expandArgs :: [String] -> [String]
expandArgs []                = []
expandArgs ("--":rest)       = "--" : rest
expandArgs (('-':ds):rest)
    | not (null ds) && all isDigit ds = "-n" : ds : expandArgs rest
expandArgs (x:rest)          = x : expandArgs rest

parseNumber :: String -> Either String Int
parseNumber s
    | not (null s) && all isDigit s = Right $ read s
    | otherwise                     = Left $ s <> " is not a number"

optionDesc :: [OptDescr (Options -> Either String Options)]
optionDesc =
    [ Option "n" ["bytes"]
        (ReqArg
            (\arg opt -> (\v -> opt { optLength = v }) <$> parseNumber arg)
            "NUMBER")
        "Minimum string length (default 4)"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "strings" optionDesc))
        "Show this help text"
    ]
