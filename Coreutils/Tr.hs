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
import qualified Data.Text             as T
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

squeeze :: ByteString -> ByteString
squeeze = C.pack . map C.head . C.group

truncate' :: ByteString -> ByteString -> ByteString
truncate' f = C.take (C.length f)

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


-- | Set Parsing

parse :: String -> ByteString
parse "[:alnum:]"   = B.filter isAlphaNum ascii
parse "[:alpha:]"   = B.filter isAlpha ascii
parse "[:blank:]"   = B.filter isSpace ascii
parse "[:cntrl:]"   = B.filter isControl ascii
parse "[:digit:]"   = B.filter isDigit ascii
parse "[:lower:]"   = B.filter isLower ascii
parse "[:print:]"   = B.filter isPrint ascii
parse "[:punct:]"   = B.filter isPunctuation ascii
parse "[:space:]"   = B.filter isSpace ascii
parse "[:upper:]"   = B.filter isUpper ascii
parse "[:graph:]"   = B.filter (\b -> isPrint b && not (isSpace b)) ascii
parse "[:xdigit:]"  = B.filter isHexDigit ascii

parse ['=', a, '='] = C.pack [a]
parse [a, '-', b]   = C.pack [a..b]
-- parse (a:'*':xs)    = undefined
parse arg           = C.pack $ interpret arg

interpret :: String -> String
-- backslash interpretation
interpret = withString convert
    where
        withString f = T.unpack . f . T.pack
        convert xs = foldr (\(from, to) ts -> T.replace from to ts) xs pairs
        pairs = [
                ("\\\\", "\\")
              , ("\\a", "\a")
              , ("\\b", "\b")
              , ("\\f", "\f")
              , ("\\n", "\n")
              , ("\\r", "\r")
              , ("\\t", "\t")
              , ("\\v", "\v")
            ]

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
