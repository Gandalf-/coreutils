{-# LANGUAGE OverloadedStrings #-}

module Coreutils.Nl where

import           Control.Monad
import           Data.Char             (isDigit)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as L
import qualified Data.Text.Lazy.IO     as L
import           System.Console.GetOpt
import           System.Exit
import           Text.Regex.TDFA       ((=~))

import           Coreutils.Util

data Nl = Nl

instance Util Nl where
    run _ = nlMain

-- | IO

nlMain :: [String] -> IO ()
nlMain args = do
        unless (null errors) $
            die $ unlines errors
        either die (`runNl` other) $
            foldM (flip id) defaultOptions opts
    where
        (opts, other, errors) = getOpt RequireOrder optionDesc args

runNl :: Options -> [String] -> IO ()
runNl os [] = runNl os ["-"]
runNl os fs = mapM_ (fetch >=> nl os) fs
    where
        fetch :: FilePath -> IO L.Text
        fetch "-" = L.getContents
        fetch f   = L.readFile f

nl :: Options -> L.Text -> IO State
nl os = foldM go initial . map L.toStrict . L.lines
    where
        go :: State -> Text -> IO State
        go st t = do
            let (new, out) = execute st t
            L.putStrLn $ L.fromStrict out
            return new

        initial = getState $ getRuntime os

-- | Implementation

type Line = Text

execute :: State -> Line -> (State, Line)
execute st s = (new, prefix <> s)
    where
        blank = T.null s
        matched
            | blank && skipBlank rt newBlanks = False
            | otherwise                       = select rt (position st) s
        prefix
            | matched   = number rt (value st)
            | otherwise = noNumber rt
        newValue
            | matched   = increment rt $ value st
            | otherwise = value st
        newBlanks
            | blank     = blanks st + 1
            | otherwise = 0
        new = State {
              position = position st
            , value    = newValue
            , blanks   = newBlanks
            , runtime  = rt
        }
        rt = runtime st

data Section = Header | Body | Footer
    deriving (Eq, Show)

data State = State {
      position :: !Section
    , value    :: !Int
    , blanks   :: !Int
    , runtime  :: !Runtime
    }

getState :: Runtime -> State
getState rt = State {
      position = Body
    , value    = start rt
    , blanks   = 0
    , runtime  = rt
    }

data Runtime = Runtime {
      number    :: !(Int -> Line)
    , noNumber  :: !Line
    , start     :: !Int
    , increment :: !(Int -> Int)
    , select    :: !(Section -> Line -> Bool)
    , section   :: !(Line -> Maybe Section)
    , skipBlank :: !(Int -> Bool)
    }

getRuntime :: Options -> Runtime
getRuntime os = Runtime {
          number    = numberer
        , noNumber  = noNumberer
        , start     = starter
        , increment = incrementer
        , select    = selecter
        , section   = sectioner
        , skipBlank = blanker
        }
    where
        sectioner _ = Nothing -- TODO
        blanker n = n < optJoinBlankLines os

        starter = optStartingLineNumber os

        selecter Header = match (optHeaderNumbering os)
        selecter Body   = match (optBodyNumbering os)
        selecter Footer = match (optFooterNumbering os)

        incrementer = (+ optLineIncrement os)

        noNumberer = T.replicate (optNumberWidth os + T.length sep) " "
        numberer i = format (optNumberFormat os) (optNumberWidth os) i <> sep
        sep = T.pack $ optNumberSeparator os

format :: Format -> NumberWidth -> Int -> Line
format f w i = case f of
        LeftNoZeros  -> num <> T.replicate size " "
        RightNoZeros -> T.replicate size " " <> num
        RightZeros   -> T.replicate size "0" <> num
    where
        size = w - T.length num
        num = T.pack $ show i

match :: Style -> Line -> Bool
match AllLines _       = True
match NonEmptyLines s  = not $ T.null s
match NoLines _        = False
match (RegexLines t) s = s =~ t


-- | Options
-- https://www.ibm.com/docs/en/aix/7.2?topic=n-nl-command
-- 130 lines of options, yikes

type Delimiter = String
type NumberWidth = Int

data Style = AllLines | NonEmptyLines | NoLines | RegexLines Text
    deriving (Eq, Show)

data Format = LeftNoZeros | RightNoZeros | RightZeros
    deriving (Eq, Show)

data Options = Options {
      optHeaderNumbering    :: Style
    , optBodyNumbering      :: Style
    , optFooterNumbering    :: Style

    , optLineIncrement      :: Int
    , optJoinBlankLines     :: Int
    , optStartingLineNumber :: Int

    , optSectionDelimiter   :: Delimiter
    , optRenumberSections   :: Bool

    , optNumberSeparator    :: String
    , optNumberFormat       :: Format
    , optNumberWidth        :: NumberWidth
    }
    deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options {
      optBodyNumbering = NonEmptyLines
    , optFooterNumbering = NoLines
    , optHeaderNumbering = NoLines

    , optLineIncrement = 1
    , optJoinBlankLines = 1
    , optStartingLineNumber = 1

    , optSectionDelimiter = "\\:"
    , optRenumberSections = True

    , optNumberSeparator = "\t"
    , optNumberFormat = RightNoZeros
    , optNumberWidth = 6
    }

getStyle :: String -> Either String Style
getStyle "a"      = Right AllLines
getStyle "t"      = Right NonEmptyLines
getStyle "n"      = Right NoLines
getStyle ('p':xs) = Right $ RegexLines $ T.pack xs
getStyle s        = Left $ s <> " is not a valid style"

getNumber :: String -> Either String Int
getNumber s
    | all isDigit s = Right $ read s
    | otherwise     = Left $ s <> " is not a number"

getFormat :: String -> Either String Format
getFormat "ln" = Right LeftNoZeros
getFormat "rn" = Right RightNoZeros
getFormat "rz" = Right RightZeros
getFormat s    = Left $ s <> " is not a valid format"

optionDesc :: [OptDescr (Options -> Either String Options)]
optionDesc =
    [ Option "b" ["body-numbering"]
        (ReqArg
            (\arg opt -> (\f -> opt { optBodyNumbering = f }) <$> getStyle arg)
            "STYLE")
        "use STYLE for numbering body lines"

    , Option "d" ["section-delimiter"]
        (ReqArg
            (\arg opt -> Right opt { optSectionDelimiter = arg })
            "CC")
        "use CC for logical page delimiters"

    , Option "f" ["footer-numbering"]
        (ReqArg
            (\arg opt -> (\f -> opt { optFooterNumbering = f }) <$> getStyle arg)
            "STYLE")
        "use STYLE for numbering footer lines"

    , Option "h" ["header-numbering"]
        (ReqArg
            (\arg opt -> (\f -> opt { optHeaderNumbering = f }) <$> getStyle arg)
            "STYLE")
        "use STYLE for numbering header lines"

    , Option "i" ["line-increment"]
        (ReqArg
            (\arg opt -> (\f -> opt { optLineIncrement = f }) <$> getNumber arg)
            "NUMBER")
        "line number increment at each line"

    , Option "l" ["join-blank-lines"]
        (ReqArg
            (\arg opt -> (\f -> opt { optJoinBlankLines = f }) <$> getNumber arg)
            "NUMBER")
        "group of NUMBER empty lines counted as one"

    , Option "n" ["number-format"]
        (ReqArg
            (\arg opt -> (\f -> opt { optNumberFormat = f }) <$> getFormat arg)
            "FORMAT")
        "insert line numbers according to FORMAT"

    , Option "p" ["no-renumber"]
        (NoArg
            (\opt -> Right opt { optRenumberSections = False }))
        "show this help text"

    , Option "s" ["number-separator"]
        (ReqArg
            (\arg opt -> Right opt { optNumberSeparator = arg })
            "STRING")
        "add STRING after possible line number"

    , Option "v" ["starting-line-number"]
        (ReqArg
            (\arg opt -> (\f -> opt { optStartingLineNumber = f }) <$> getNumber arg)
            "NUMBER")
        "first line number after each section"

    , Option "w" ["number-width"]
        (ReqArg
            (\arg opt -> (\f -> opt { optNumberWidth = f }) <$> getNumber arg)
            "NUMBER")
        "use NUMBER columns for line numbers"

    , Option "" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "awk" optionDesc))
        "show this help text"
    ]
