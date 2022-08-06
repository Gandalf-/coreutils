module Coreutils.Nl where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C

import           Data.Char             (isDigit)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           System.Console.GetOpt
import           Text.Regex.TDFA       ((=~))

import           Coreutils.Util

data Nl = Nl

instance Util Nl where
    run _ = nlMain

nlMain :: [String] -> IO ()
nlMain = undefined

-- | Implementation

data Section = Header | Body | Footer
    deriving (Eq, Show)

execute :: State -> ByteString -> (State, ByteString)
execute st s = (new, prefix <> s)
    where
        blank = C.null s
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

data State = State {
      position :: !Section
    , value    :: !Int
    , blanks   :: !Int
    , runtime  :: Runtime
    }

getState :: Runtime -> State
getState rt = State {
      position = Body
    , value    = start rt
    , blanks   = 0
    , runtime  = rt
    }

data Runtime = Runtime {
      number    :: Int -> ByteString
    , noNumber  :: ByteString
    , start     :: Int
    , increment :: Int -> Int
    , select    :: Section -> ByteString -> Bool
    , section   :: ByteString -> Maybe Section
    , skipBlank :: Int -> Bool
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

        noNumberer = C.replicate (optNumberWidth os + 1) ' '
        numberer i = format (optNumberFormat os) (optNumberWidth os) i <> sep
        sep = C.pack $ optNumberSeparator os

format :: Format -> NumberWidth -> Int -> ByteString
format f w i = case f of
        LeftNoZeros  -> num <> C.replicate size ' '
        RightNoZeros -> C.replicate size ' ' <> num
        RightZeros   -> C.replicate size '0' <> num
    where
        size = w - C.length num
        num = C.pack $ show i

match :: Style -> ByteString -> Bool
match AllLines _       = True
match NonEmptyLines s  = not $ C.null s
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
        "use CC for logical line delimiters"

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
