module Coreutils.Jot where

import           Coreutils.Util
import           Data.Char
import           System.Console.GetOpt
import           Text.Read

data Jot = Jot
instance Util Jot where
    run _ _ = undefined


data Generator = Sequential | Random | Word String
    deriving (Show, Eq)

data Range = Range
    { reps  :: Integer
    , rLow  :: Double
    , rHigh :: Double
    , rStep :: Double
    }
    deriving (Show, Eq)

defaultRange :: Range
defaultRange = Range 100 1 100 1

data Options = Options
    { optGenerator    :: Generator
    , optFormat       :: Maybe String
    , optSeparator    :: String
    , optFinalNewline :: Bool
    , optPrecision    :: Maybe Integer
    }
    deriving (Show, Eq)

normalizeRange :: [String] -> Either String (String, String, String, String)
normalizeRange []           = Left $ usageInfo "jot" options
normalizeRange [a]          = Right (a, "-", "-", "-")
normalizeRange [a, b]       = Right (a,   b, "-", "-")
normalizeRange [a, b, c]    = Right (a,   b,   c, "-")
normalizeRange [a, b, c, d] = Right (a,   b,   c,   d)
normalizeRange _            = Left "Too many arguments"

parseRange :: [String] -> Either String Range
parseRange xs = do
    (a, b, c, d) <- normalizeRange xs
    reps  <- parseDefault parseInteger 100 a

    rLow  <- parseDefault parseBound     1 b
    rHigh <- parseDefault parseBound   100 c

    let dStep = if rLow < rHigh then 1 else -1
    rStep <- parseDefault parseDouble  dStep d

    Right $ Range {..}

parseDefault :: (String -> Either String a) -> a -> String -> Either String a
parseDefault _     d ""  = Right d
parseDefault _     d "-" = Right d
parseDefault parse _ xs  = parse xs

parseBound :: String -> Either String Double
-- ASCII character or double
parseBound xs = case parseDouble xs of
    Left _ -> case xs of
        [c] -> Right $ fromIntegral $ ord c
        _   -> Left $ xs <> " is not an ASCII character or number"
    Right b -> Right b

parseDouble :: String -> Either String Double
parseDouble ('.':xs) = parseDouble $ "0." <> xs
parseDouble xs =
    case readMaybe xs of
        Nothing  -> Left $ xs <> " is not a number"
        (Just d) -> Right d

parseInteger :: String -> Either String Integer
parseInteger xs =
    case readMaybe xs of
        Nothing  -> Left $ xs <> " is not an integer"
        (Just d) -> Right d

defaultOptions :: Options
defaultOptions = Options
    { optGenerator = Sequential
    , optFormat = Nothing
    , optSeparator = "\n"
    , optFinalNewline = True
    , optPrecision = Nothing
    }

options :: [OptDescr (Options -> Either String Options)]
options =
    [ Option "r" []
        (NoArg
            (\opt -> Right opt { optGenerator = Random }))
        "Generate data randomly rather than sequentially"

    , Option "c" []
        (NoArg
            (\opt -> Right opt { optFormat = Just "%c" }))
        "This is an abbreviation for `-w %c`"

    , Option "b" []
        (ReqArg
            (\arg opt -> Right opt { optGenerator = Word arg })
            "word")
        "Just print `word` repeatedly"

    , Option "w" []
        (ReqArg
            (\arg opt -> Right opt { optFormat = Just arg })
            "word")
        "Just `word` with the generated data appended to it, printf format capable"

    , Option "s" []
        (ReqArg
            (\arg opt -> Right opt { optSeparator = arg })
            "string")
        "Separate output with `string` instead of a newline"

    , Option "n" []
        (NoArg
            (\opt -> Right opt { optFormat = Just "%c" }))
        "Do not print a final newline"

    , Option "p" []
        (ReqArg
            (\arg opt -> (\v -> opt { optPrecision = Just v }) <$> parseInteger arg)
            "string")
        "Separate output with `string` instead of a newline"
    ]
