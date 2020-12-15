{-# LANGUAGE OverloadedStrings #-}

module Coreutils.Tr where

-- tr
--
-- replace characters, supports -d and -c

import           Control.Monad
import qualified Data.ByteString.Char8 as C
import           Data.Char
import qualified Data.HashMap.Strict   as H
import qualified Data.Text             as T
import           System.Console.GetOpt
import           System.Exit
import           Text.Read

import           Coreutils.Util

type Set = String

data Tr = Tr

type TrFunc = (C.ByteString -> C.ByteString)

instance Util Tr where
    run _ = trMain

data Action = Delete | Translate
    deriving (Show, Eq)

data Options = Options {
          optComplement :: Bool
        , optAction     :: Action
        , optSqueeze    :: Bool
        , optTruncate   :: Bool
    }
    deriving (Show, Eq)

trMain :: [String] -> IO ()
trMain args = do
        unless (null errors) $
            die $ unlines errors

        either die (`runner` arguments) $
            foldM (flip id) defaultOptions opts
    where
        (opts, arguments, errors) = getOpt RequireOrder optionDesc args
        runner o as = either die C.interact $ runTr o as

runTr :: Options -> [String] -> Either String TrFunc
runTr o [s]    = tr o s []
runTr o [s, t] = tr o s t
runTr _ _      = Left "exactly one or two sets may be provided"

tr :: Options -> Set -> Set -> Either String TrFunc
tr o s1 s2 = case (optAction o, s2) of
        (Delete, [])    -> maybeEither "error" (trDelete <$> squeeze set1)
        (Delete, _ )    -> Left "--delete does not allow a second SET"
        (Translate, []) -> Left "translation requires two SETs"
        (Translate, _ ) -> maybeEither "error" (trTranslate <$> set1 <*> squeeze set2)
    where
        maybeEither e Nothing  = Left e
        maybeEither _ (Just v) = Right v
        squeeze = whenF (optSqueeze o) setSqueeze

        set1 =
            whenF (optTruncate o) (`setTruncate` s2)
            $ whenF (optComplement o) setComplement
            $ parse
            $ interpret s1

        set2 =
            parse
            $ interpret s2

whenF :: Applicative f => Bool -> (a -> a) -> f a -> f a
whenF True  f = fmap f
whenF False _ = id

trDelete :: Set -> TrFunc
trDelete s =
        C.filter (`C.notElem` set)
    where
        set = C.pack s

trTranslate :: Set -> Set -> TrFunc
trTranslate s1 s2 =
        C.map translate
    where
        table :: H.HashMap Char Char
        table = H.fromList $ zip s1 (s2 <> repeat (last s2))

        translate c = H.lookupDefault c c table

-- | Set Processing

setSqueeze :: String -> String
setSqueeze [] = []
setSqueeze [x] = [x]
setSqueeze (x:y:xs)
        | x == y    = x : setSqueeze xs
        | otherwise = x : setSqueeze (y:xs)

setComplement :: String -> String
setComplement set = filter (`notElem` set) ascii

setTruncate :: Set -> Set -> Set
setTruncate s1 s2 = take (length s2) s1

-- | Set Parsing

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

parse :: String -> Maybe Set
parse "[:alnum:]"   = Just $ filter isAlphaNum ascii
parse "[:alpha:]"   = Just $ filter isAlpha ascii
parse "[:blank:]"   = Just $ filter isSpace ascii    -- suspicious
parse "[:cntrl:]"   = Just $ filter isControl ascii
parse "[:digit:]"   = Just digit
parse "[:graph:]"   = Just $ filter isPrint ascii
parse "[:lower:]"   = Just lower
parse "[:print:]"   = undefined
parse "[:punct:]"   = Just $ filter isPunctuation ascii
parse "[:space:]"   = Just $ filter isSpace ascii
parse "[:upper:]"   = Just upper
parse "[:xdigit:]"  = undefined

parse ['=', a, '='] = Just [a]
parse [a, '-', b]   = Just [a..b]
parse (a:'*':xs)    = flip replicate a <$> readMaybe xs
parse arg           = Just arg

upper :: String
upper = ['A'..'Z']

lower :: String
lower = ['a'..'z']

digit :: String
digit = ['0'..'9']

ascii :: String
ascii = map chr [0..255]

-- | Options

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
