module Coreutils.Tr where

-- tr
--
-- replace characters, supports -d and -c

import           Data.Char       (chr, isControl, isPrint, isPunctuation,
                                  isSpace)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)
import           System.Exit

import           Coreutils.Util

type Table = Map.Map Char Char

data Translator = Translator String String
newtype Deleter = Deleter String


class Action a where

        translate :: a -> Bool -> String -> String


instance Action Translator where

        translate (Translator from to) False s =
                    map (search table) s
                where table = buildTable from to

        -- complement
        translate (Translator from to) True s =
                    map (invertSearch table) s
                where table = buildTable from to


instance Action Deleter where

        translate (Deleter word) False s  =
                    filter (`notElem` word) s

        -- complement
        translate (Deleter word) True s =
                    filter (`elem` word) s


data Argument = Complement
              | Delete
              | Set String
              deriving (Eq, Show)


runner :: [Argument] -> String -> String
runner args content
        | Delete `elem` args = deleterResult
        | otherwise          = translatorResult
    where
        deleterResult =
          translate (Deleter first) complement content

        translatorResult =
          translate (Translator first second) complement content

        complement = Complement `elem` args

        sets' = sets args

        first  = head sets'
        second = head $ tail sets'


data ValidationResult = NoErrors | Error String

validate :: [Argument] -> ValidationResult
validate args
        | checkDelete = Error "tr: -d requires one set"
        | checkTransl = Error "tr: requires two sets"
        | otherwise   = NoErrors
    where
        sets' = sets args
        checkDelete = Delete `elem`    args && length sets' /= 1
        checkTransl = Delete `notElem` args && length sets' /= 2


buildTable :: String -> String -> Table
buildTable from to = Map.fromList $ zip from $ cycle to

search :: Table -> Char -> Char
search t c = fromMaybe c $ Map.lookup c t


invertSearch :: Table -> Char -> Char
invertSearch table c =
        if Map.member c table
                then c
                else head $ Map.elems table


parse :: String -> Argument
parse "-c"           = Complement
parse "-C"           = Complement
parse "--complement" = Complement

parse "-d"           = Delete
parse "--delete"     = Delete

parse "[:alnum:]"    = Set $ upper ++ lower ++ digit
parse "[:alpha:]"    = Set $ upper ++ lower
parse "[:upper:]"    = Set upper
parse "[:lower:]"    = Set lower
parse "[:digit:]"    = Set digit

parse "[:blank:]"    = Set $ filter isSpace ascii
parse "[:cntrl:]"    = Set $ filter isControl ascii
parse "[:graph:]"    = Set $ filter isPrint ascii
parse "[:punct:]"    = Set $ filter isPunctuation ascii

parse [a, '-', b]    = Set [a..b]

parse arg            = Set arg


upper :: String
upper = ['A'..'Z']

lower :: String
lower = ['a'..'z']

digit :: String
digit = ['0'..'9']

ascii :: String
ascii = map chr [0..127]


sets :: [Argument] -> [String]
sets []          = []
sets (Set s: xs) = s : sets xs
sets (_ : xs)    = sets xs

data Tr = Tr

instance Util Tr where
     run _ args =
            case validate arguments of
                NoErrors  -> interact (runner arguments)
                Error msg -> die msg
        where
            arguments = map parse args
