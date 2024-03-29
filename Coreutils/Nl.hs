{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Coreutils.Nl where

import           Control.Monad
import           Control.Monad.State.Strict
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as C
import           Data.Char                  (isDigit)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Streaming
import qualified Streaming.ByteString.Char8 as Q
import qualified Streaming.Prelude          as S
import           System.Console.GetOpt
import           System.Exit
import           System.IO
import           Text.Regex.TDFA            ((=~))

import           Coreutils.Util
import           Data.Maybe

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
runNl os fs = mapM_ runner fs
    where
        runner :: FilePath -> IO ()
        runner "-" = nl os Q.stdin
        runner f   = withFile f ReadMode (nl os . Q.fromHandle)

nl :: Options -> Q.ByteStream Op () -> IO ()
nl os bs = void $ worker Q.stdout bs initial
    where
        initial = getState $ getRuntime os

type Op = StateT NlState IO

worker :: (Q.ByteStream Op () -> Op a) -> Q.ByteStream Op () -> NlState -> IO (a, NlState)
worker sink bs = runStateT (sink $ go bs)
    where
        go = Q.unlines . S.subst Q.chunk . S.mapM process . mapped Q.toStrict . Q.lines

process :: Line -> Op Line
process l = do
    st <- get
    let (!new, !line) = execute st l
    put $! new
    return line

-- | Implementation

type Line = ByteString

execute :: NlState -> Line -> (NlState, Line)
execute !st !s = (newState, newLine)
    where
        blank = C.null s
        newLine
            | isJust newSection = prefix
            | otherwise         = prefix <> s
        matched
            | blank && skipBlank rt newBlanks = False
            | isJust newSection               = False
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
        newSection = section rt s
        newState = NlState {
              position = fromMaybe (position st) newSection
            , value    = newValue
            , blanks   = newBlanks
            , runtime  = rt
        }
        rt = runtime st

data Section = Header | Body | Footer
    deriving (Eq, Show)

data NlState = NlState {
      position :: Section
    , value    :: Int
    , blanks   :: Int
    , runtime  :: Runtime
    }

instance Show NlState where
    show (NlState p v b _) = unwords [show p, show v, show b]

getState :: Runtime -> NlState
getState rt = NlState {
      position = Body
    , value    = start rt
    , blanks   = 0
    , runtime  = rt
    }

data Runtime = Runtime {
      number    :: Int -> Line
    , noNumber  :: Line
    , start     :: Int
    , increment :: Int -> Int
    , select    :: Section -> Line -> Bool
    , section   :: Line -> Maybe Section
    , skipBlank :: Int -> Bool
    }

getRuntime :: Options -> Runtime
getRuntime os = Runtime { .. }
    where
        section = getSection (optSectionDelimiter os)
        skipBlank n = n < optJoinBlankLines os

        start = optStartingLineNumber os

        select Header = match (optHeaderNumbering os)
        select Body   = match (optBodyNumbering os)
        select Footer = match (optFooterNumbering os)

        increment = (+ optLineIncrement os)

        noNumber = C.replicate (optNumberWidth os + C.length sep) ' '
        number i = format (optNumberFormat os) (optNumberWidth os) i <> sep
        sep = C.pack $ optNumberSeparator os

format :: Format -> NumberWidth -> Int -> Line
format f w i = case f of
        LeftNoZeros  -> num <> C.replicate size ' '
        RightNoZeros -> C.replicate size ' ' <> num
        RightZeros   -> C.replicate size '0' <> num
    where
        size = w - C.length num
        num = C.pack $ show i

match :: Style -> Line -> Bool
match AllLines _       = True
match NonEmptyLines s  = not $ C.null s
match NoLines _        = False
match (RegexLines t) s = s =~ t

getSection :: Delimiter -> Line -> Maybe Section
getSection d l
    | d == l           = Just Footer
    | d <> d == l      = Just Body
    | d <> d <> d == l = Just Header
    | otherwise = Nothing


-- | Options
-- https://www.ibm.com/docs/en/aix/7.2?topic=n-nl-command
-- 130 lines of options, yikes

type Delimiter = ByteString
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
            (\arg opt -> Right opt { optSectionDelimiter = C.pack arg })
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
            (\_ -> Left $ usageInfo "nl" optionDesc))
        "show this help text"
    ]
