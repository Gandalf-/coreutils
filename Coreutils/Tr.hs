{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Coreutils.Tr where

-- tr
--
-- replace characters, supports -d and -c

import           Control.Monad
import           Data.Array
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C
import           Data.Word8
import           Streaming
import qualified Streaming.ByteString  as Q
import           System.Console.GetOpt
import           System.Exit

import           Coreutils.Util

data Tr = Tr

instance Util Tr where
    run _ = trMain

-- | IO

trMain :: [String] -> IO ()
trMain args = do
        unless (null errors) $
            die $ unlines errors

        either die (`runner'` arguments) $
            foldM (flip id) defaultOptions opts
    where
        (opts, arguments, errors) = getOpt RequireOrder optionDesc args

runner' :: Options -> [String] -> IO ()
runner' o as = case prepare o sets of
        (Left err)  -> die err
        (Right exe) -> Q.stdout $ execute exe Q.stdin
    where
        sets = map parse as


-- | Implementation

type Complement = Bool

range' :: [Word8]
range' = [0..255]

bounds' :: (Word8, Word8)
bounds' = (0, 255)

data Translator =
      Translator (Array Word8 Word8)
    | Deleter    (Array Word8 Bool)
    deriving (Show, Eq)

translationTable :: Complement -> ByteString -> ByteString -> Translator
-- map everything not in `from` to the last character in `to`
translationTable True f t = Translator out
    where
        out     = base // fmap (, final) notFrom
        base    = array bounds' $ zip range' range'
        notFrom = filter (`notElem` from) range'
        from    = B.unpack f
        final   = B.last t
-- map everything in `from` to `to`
translationTable False f t = Translator out
    where
        out  = base // zip from to
        base = array bounds' $ zip range' range'
        from = B.unpack f
        to   = B.unpack t

deletionTable :: Complement -> ByteString -> Translator
deletionTable c r
        | c         = Deleter (not <$> out)
        | otherwise = Deleter out
    where
        out    = base // fmap (, False) remove
        base   = array bounds' $ zip range' $ repeat True
        remove = B.unpack r

execute :: MonadIO m => Translator -> Q.ByteStream m () -> Q.ByteStream m ()
execute (Translator t) = Q.map (t !)
execute (Deleter t)    = Q.filter (t !)


-- | Preparation

prepare :: Options -> [ByteString] -> Either String Translator
prepare _                      []     = Left "At least one set must be provided"
prepare o@(Options True _ _ _) [a]    = prepare o { optSqueeze  = False } [squeeze a]
prepare o@(Options True _ _ _) [a, b] = prepare o { optSqueeze  = False } [a, squeeze b]
prepare o@(Options _ True _ _) [a, b] = prepare o { optTruncate = False } [a, truncate' a b]

prepare (Options False False c Translate) [a, b] = Right $ translationTable c a b
prepare (Options False False c Delete   ) [a]    = Right $ deletionTable c a

prepare (Options _ _ _ Translate) [_] = Left "Translation requires two sets"
prepare (Options _ _ _ Delete)  (_:_) = Left "Deletion requires one set"
prepare _                      _      = Left "Invalid options"

squeeze :: ByteString -> ByteString
squeeze = C.pack . map C.head . C.group

truncate' :: ByteString -> ByteString -> ByteString
truncate' f = C.take (C.length f)


-- | Set Parsing

parse :: String -> ByteString
parse ('[':':':'a':'l':'n':'u':'m':':':']':xs)     = next isAlphaNum xs
parse ('[':':':'a':'l':'p':'h':'a':':':']':xs)     = next isAlpha xs
parse ('[':':':'d':'i':'g':'i':'t':':':']':xs)     = next isDigit xs
parse ('[':':':'u':'p':'p':'e':'r':':':']':xs)     = next isUpper xs
parse ('[':':':'l':'o':'w':'e':'r':':':']':xs)     = next isLower xs
parse ('[':':':'s':'p':'a':'c':'e':':':']':xs)     = next isSpace xs
parse ('[':':':'p':'r':'i':'n':'t':':':']':xs)     = next isPrint xs
parse ('[':':':'p':'u':'n':'c':'t':':':']':xs)     = next isPunctuation xs
parse ('[':':':'b':'l':'a':'n':'k':':':']':xs)     = next isSpace xs
parse ('[':':':'c':'n':'t':'r':'l':':':']':xs)     = next isControl xs
parse ('[':':':'g':'r':'a':'p':'h':':':']':xs)     = next isGraph xs
parse ('[':':':'x':'d':'i':'g':'i':'t':':':']':xs) = next isHexDigit xs

parse ('=':a:'=':xs)                               = pack [a] xs
parse (a:'-':b:xs)                                 = pack [a..b] xs
parse (a:'*':xs)                                   = C.replicate (read xs) a

parse ('\\':'\\':xs)                               = pack "\\" xs
parse ('\\':'a':xs)                                = pack "\a" xs
parse ('\\':'b':xs)                                = pack "\b" xs
parse ('\\':'f':xs)                                = pack "\f" xs
parse ('\\':'n':xs)                                = pack "\n" xs
parse ('\\':'r':xs)                                = pack "\r" xs
parse ('\\':'t':xs)                                = pack "\t" xs
parse ('\\':'v':xs)                                = pack "\v" xs

parse (x:xs)                                       = C.singleton x <> parse xs
parse []                                           = B.empty

next :: (Word8 -> Bool) -> String -> ByteString
next p xs = B.filter p ascii <> parse xs

pack :: String -> String -> ByteString
pack s xs = C.pack s <> parse xs

isGraph :: Word8 -> Bool
isGraph b = isPrint b && not (isSpace b)

ascii :: ByteString
ascii = B.pack range'


-- | Options

data Action = Delete | Translate
    deriving (Show, Eq)

data Options = Options {
          optSqueeze    :: Bool
        , optTruncate   :: Bool
        , optComplement :: Bool
        , optAction     :: Action
    }
    deriving (Show, Eq)

defaultOptions :: Options
defaultOptions = Options {
          optComplement = False
        , optAction = Translate
        , optSqueeze = False
        , optTruncate = False
    }

optionDesc :: [OptDescr (Options -> Either String Options)]
optionDesc =
    [ Option "cC" ["complement"]
        (NoArg
            (\opt -> Right opt { optComplement = True }))
        "use the complement of SET1"

    , Option "d" ["delete"]
        (NoArg
            (\opt -> Right opt { optAction = Delete }))
        "delete characters in SET1, do not translate"

    , Option "s" ["squeeze-repeats"]
        (NoArg
            (\opt -> Right opt { optSqueeze = True }))
        "combine repeated encountered characters from the last SET"

    , Option "t" ["truncate-set1"]
        (NoArg
            (\opt -> Right opt { optTruncate = True }))
        "first truncate SET1 to length of SET2"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "tr [OPTION]... SET1 [SET2]" optionDesc))
        "show this help text"
    ]
