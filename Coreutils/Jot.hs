module Coreutils.Jot where

import           Control.Monad
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
    { start :: Double
    , step  :: Double
    , count :: Integer
    }
    deriving (Show, Eq)

-- | Parsing

jotParse :: (Maybe Integer, Maybe Double, Maybe Double, Maybe Double) -> Either String Range
jotParse (Nothing, Nothing, Nothing, Nothing) =
    Left "One value must be provided"

-- | Missing three values
jotParse (Just n,  Nothing, Nothing, Nothing) = Right $ Range 1         1  n
jotParse (Nothing, Just lb, Nothing, Nothing) = Right $ Range lb        1  100
jotParse (Nothing, Nothing, Just ub, Nothing) = Right $ Range (ub - 99) 1  100
jotParse (Nothing, Nothing, Nothing, Just ss) = Right $ Range 1         ss 100

-- | Missing two values
jotParse (Nothing, lb, ub, Nothing) = jotParse (Nothing, lb, ub, Just 1)
jotParse (n,  lb, Nothing, Nothing) = jotParse (n,  lb, Nothing, Just 1)
jotParse (n,  Nothing, ub, Nothing) = jotParse (n,  Nothing, ub, Just 1)

-- !
jotParse (Nothing, lb, Nothing, ss) = jotParse (Just 100, lb, Nothing, ss)
jotParse (Nothing, Nothing, ub, ss) = jotParse (Just 100, Nothing, ub, ss)
jotParse (n, Nothing, Nothing, ss)  = jotParse (n, Just 1, Nothing, ss)

-- | Missing a single value
jotParse (Nothing, Just lb, Just ub, Just ss) = do
    -- Missing count
    let n = 1 + floor ((ub - lb) / ss)
    Right $ Range lb ss n

jotParse (Just n, Nothing, Just ub, Just ss) = do
    -- Missing lower
    let lb = ss + ub - fromIntegral n * ss
    jotParse (Just n, Just lb, Just ub, Just ss)

jotParse (Just n, Just lb, Nothing, Just ss) = do
    -- Missing upper
    let ub = lb + fromIntegral n * ss
    jotParse (Just n, Just lb, Just ub, Just ss)

jotParse (Just n, Just lb, Just ub, Nothing) = do
    -- Missing step
    let ss = if lb <= ub then 1 else (-1)
    jotParse (Just n, Just lb, Just ub, Just ss)

-- | Missing nothing
jotParse (Just n, Just lb, Just ub, Just ss) = do
    let count = min n (1 + floor ((ub - lb) / ss))
    when (count < 0) $
        Left "Range must be non-negative"
    Right $ Range lb ss count


parseRange :: [String] -> Either String Range
parseRange xs = do
        n  <- parseMaybe parseInteger sn
        lu <- parseMaybe parseBound sl
        lb <- parseMaybe parseBound su
        ss <- parseMaybe parseDouble sc
        jotParse (n, lu, lb, ss)
    where
        [sn, sl, su, sc] = take 4 (xs <> repeat "")

parseMaybe :: (String -> Either String a) -> String -> Either String (Maybe a)
parseMaybe _ ""  = Right Nothing
parseMaybe _ "-" = Right Nothing
parseMaybe f xs  = Just <$> f xs

parseBound :: String -> Either String Double
-- ASCII character or double
parseBound xs = case parseDouble xs of
    Left _ -> case xs of
        [c] -> Right $ fromIntegral $ ord c
        _   -> Left $ xs <> " is not an ASCII character or number"
    Right b -> Right b

parseDouble :: String -> Either String Double
parseDouble ('-':'.':xs) = parseDouble $ "-0." <> xs
parseDouble ('.':xs)     = parseDouble $ "0." <> xs
parseDouble xs =
    case readMaybe xs of
        Nothing  -> Left $ xs <> " is not a number"
        (Just d) -> Right d

parseInteger :: String -> Either String Integer
parseInteger xs =
    case readMaybe xs of
        Nothing  -> Left $ xs <> " is not an integer"
        (Just d) -> Right d

-- | Options

data Options = Options
    { optGenerator    :: Generator
    , optFormat       :: Maybe String
    , optSeparator    :: String
    , optFinalNewline :: Bool
    , optPrecision    :: Maybe Integer
    }
    deriving (Show, Eq)

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
