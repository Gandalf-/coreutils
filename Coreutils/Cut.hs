{-# LANGUAGE BangPatterns #-}

module Coreutils.Cut where

import           Data.List
import           Data.List.Split                 (splitOn)
import           Data.Maybe
import qualified Data.Text.Lazy                  as T
import qualified Data.Text.Lazy.IO               as T
import           System.Exit                     (die)
import           System.IO
import           Text.Read                       (readMaybe)

import           Coreutils.Util


-- | Data types

data Field =
          Range Double Double
        | Exact Double
    deriving (Show, Eq)

data Selector =
          Bytes  [Field]
        | Chars  [Field]
        | Fields [Field]
        | Invalid
    deriving (Show, Eq)

data Options = Options
        { delimiter       :: Char         -- use this character to divide fields
        , selector        :: Selector     -- only select these elements
        , complement      :: Bool         -- take the complement of bytes, characters or fields?
        , onlyDelimited   :: Bool         -- only print lines containing delimiter?
        , outputDelimiter :: Maybe String -- use this to combine output elements
        , inputs          :: [FilePath]   -- input files, empty means stdin
        }
    deriving (Show, Eq)

instance Ord Field where
    compare (Exact a)   (Exact b)   = compare a b
    compare (Exact a)   (Range b _) = compare a b
    compare (Range a _) (Exact b)   = compare a b
    compare (Range a _) (Range b _) = compare a b


-- | Business logic

cut :: [Field] -> [a] -> [a]
-- ^ an element is accepted if its index falls within the range of any of the fields
-- provided. all other elements are discarded
cut fs = fastCut fs 1


fastCut :: [Field] -> Double -> [a] -> [a]
fastCut [] _ _ = []
fastCut _ _ [] = []
fastCut (Range low high:fs) i (x:xs)
        | low > i   =
            -- jump ahead to the lower bound
            fastCut (Range low high:fs) low $! drop jump (x:xs)

        | low == i  =
            if high == infinity
                -- everything else!
                then x:xs

                -- grab `high - low + 1` elements
                else take (shift + 1) (x:xs) <> fastCut fs (high + 1) (drop shift xs)

        | high < i  =
            -- this range is irrelevant
            fastCut fs i (x:xs)

        | otherwise =
            -- low < i <= high, truncate the range's lower bound to our current position
            fastCut (Range i high:fs) i (x:xs)
    where
        shift = round high - round low :: Int
        jump  = round low  - round i   :: Int

fastCut (Exact e:fs) i (x:xs)
        | i < e     =
            -- jump ahead to the exact value
            fastCut (Exact e:fs) e $! drop jump (x:xs)

        | i == e    =
            -- match
            x : fastCut fs (i + 1) xs

        | otherwise =
            -- this exact field is irrelevant, we've already passed it
            fastCut fs i (x:xs)
    where
        jump = round (e - i)


cut' :: [Field] -> [a] -> [a]
-- ^ cut complement
-- if there's only one field, we can be clever by inverting it and using the fast
-- implementation. if there are multiple fields, we're stuck with the slow
-- implementation for now
cut' [f] = cut $ fieldInvert f
cut' fs  = slowCut' fs


fieldInvert :: Field -> [Field]
fieldInvert (Exact 1)   = [Range 2 infinity]
fieldInvert (Exact e)   = [Range 1 (e - 1), Range (e + 1) infinity]
fieldInvert (Range 1 b) = [Range (b + 1) infinity]
fieldInvert (Range a b)
        | b == infinity = [Range 1 (a - 1)]
        | otherwise     = [Range 1 (a - 1), Range (b + 1) infinity]


slowCut' :: [Field] -> [a] -> [a]
-- ^ an element is rejected if its index falls within the range of any of the fields
-- provided. all other elements are kept
slowCut' fs xs =
        map fst . filter allOutbounds $ zip xs [1..]
    where
        allOutbounds x = all (\f -> not . inbounds f $ snd x) fs

        inbounds :: Field -> Double -> Bool
        inbounds (Range fmin fmax) x = x >= fmin && x <= fmax
        inbounds (Exact fval) x      = x == fval


-- | IO

data Cut = Cut

instance Util Cut where
    -- ^ library hook
    run _ args =
            case parseOptions defaultOptions expandedArgs of
                Left e  -> die e
                Right o -> cutMain o
        where
            expandedArgs = concatMap parseExpand args

cutMain :: Options -> IO ()
-- ^ deals with collecting inputs
cutMain o
        | null $ inputs o   = cutInput o stdin
        | inputs o == ["-"] = cutInput o stdin
        | otherwise         = mapM_ go $ reverse $ inputs o
    where
        go :: FilePath -> IO ()
        go input = withFile input ReadMode (cutInput o)

cutInput :: Options -> Handle -> IO ()
-- run the business logic for this particular handle; particularly, choose which
-- preprocessing is necssary, then call the appropriate cut function
cutInput o h =
        case selector o of
            (Chars field) ->
                (lines <$> hGetContents h)
                >>= mapM_ (putStrLn . runChars field)

            (Fields field) ->
                (T.lines <$> T.hGetContents h)
                >>= mapM_ (filterPrint . runFields field)

            (Bytes _) ->
                die "not implemented"

            _ ->
                die "exactly one of: bytes, characters, or fields must be provided"
    where
        runChars :: [Field] -> String -> String
        runChars !f =
            case outputDelimiter o of
                Nothing ->
                    cutFunction (sort f)
                Just d  ->
                    intercalate d
                    . groupBy (const . const False)
                    . cutFunction (sort f)

        runFields :: [Field] -> T.Text -> T.Text
        runFields !f =
            T.intercalate (maybe tDelimiter T.pack (outputDelimiter o))
            . cutFunction (sort f)
            . T.splitOn tDelimiter

        tDelimiter = T.pack [delimiter o]

        filterPrint :: T.Text -> IO ()
        filterPrint !s
            | onlyDelimited o =
                if T.count tDelimiter s > 0
                    then T.putStrLn s
                    else pure ()
            | otherwise = T.putStrLn s

        cutFunction
            | complement o = cut'
            | otherwise    = cut


-- | Parsing arguments into Options

defaultOptions :: Options
defaultOptions = Options
        { delimiter = '\t'
        , selector = Invalid
        , complement = False
        , onlyDelimited = False
        , outputDelimiter = Nothing
        , inputs = []
        }

parseOptions :: Options -> [String] -> Either String Options
-- ^ argument parsing. first argument is the previous options state, since we're just
-- making updates as we come across flags. all unmatched flags are assumed to be files
parseOptions o ("-b":b:xs) = chooseSelector o b Bytes xs
parseOptions o ("--bytes":b:xs) = chooseSelector o b Bytes xs

parseOptions o ("-c":c:xs) = chooseSelector o c Chars xs
parseOptions o ("--characters":c:xs) = chooseSelector o c Chars xs

parseOptions o ("-f":f:xs) = chooseSelector o f Fields xs
parseOptions o ("--fields":f:xs) = chooseSelector o f Fields xs

parseOptions o ("-d":d:xs) = chooseDelimiter o d xs
parseOptions o ("--delimiter":d:xs) = chooseDelimiter o d xs

parseOptions o ("--complement":xs) = parseOptions (o { complement = True}) xs

parseOptions o ("-s":xs) = parseOptions (o { onlyDelimited = True}) xs
parseOptions o ("--only-delimited":xs) = parseOptions (o { onlyDelimited = True}) xs

parseOptions o ("--output-delimiter":d:xs) =
        parseOptions (o { outputDelimiter = Just d}) xs

parseOptions _ ("-h":_) = Left usage
parseOptions _ ("--help":_) = Left usage

parseOptions o (x:xs) =
        -- treat these unknown things as input files
        parseOptions (o { inputs = x : inputs o}) xs

parseOptions base [] = Right base

parseExpand :: String -> [String]
-- ^ allows short hand usage of single character flags, like -d' ', -f1,2
parseExpand o@('-':n:xs)
        | short && more = [['-', n], xs]
        | otherwise     = [o]
    where
        short = n `elem` "bcfds"
        more  = (not . null) xs

parseExpand s = [s]


-- | Argument parsing helper functions

usage :: String
usage = unlines [
     "cut: [options...] [files...]"
    , ""
    , "   display selected portions of files or stdin to stdout"
    , ""
    , "   -b, --bytes <range>           select only these bytes"
    , "   -c, --characters <range>      select only these characters"
    , "   -f, --fields <range>          select only these fields"
    , "   "
    , "   -d, --delimiter <str>         use this to define fields"
    , "   "
    , "       --complement              use the complement of the selection"
    , "   -s, --only-delimited          do not print lines without delimitors"
    , "       --output-delimiter <str>  combine elements with this value"
    , ""
    , "   -b, -c, and -f are mutally exclusive"
    , ""
    , "   ranges may be defined as follows"
    , "       -n  == [1..n]"
    , "       n-  == [n..]"
    , "       n-m == [n..m]"
    , "       n   == [n]"
    , "   several ranges may be provided, separated by a comma"
    ]


chooseDelimiter :: Options -> String -> [String] -> Either String Options
-- ^ gnu cut only allows a single character delimiter; we could do strings, but the
-- result ends up being pretty weird
chooseDelimiter o d xs
        | length d == 1 = parseOptions (o { delimiter = head d, outputDelimiter = Just [head d]}) xs
        | otherwise     = Left "the delimiter may only be a single character"


chooseSelector :: Options -> String -> ([Field] -> Selector) -> [String] -> Either String Options
chooseSelector o s f xs =
        case selector o of
            Invalid ->
                -- invalid means that a selector choice hasn't been made already
                if any isNothing fields
                    then Left $ "could not parse a field from: " <> s
                    else parseOptions (o { selector = f $ catMaybes fields}) xs
            _ ->
                Left "exactly one of: bytes, characters, or fields must be provided"
    where
        fieldElements = splitOn "," s
        fields = map parseField fieldElements


parseField :: String -> Maybe Field
-- ^ determines whether a string represents a valid field. these map pretty closely to
-- the built in list ranges, but we don't use them to avoid dealing with infinite lists
--   -n  == [1..n]
--   n-  == [n..]
--   n-m == [n..m]
--   n   == [n]
parseField f
        | null f                     = Nothing
        | splitOkay && head f == '-' = checkOrdering $ Range 1 <$> second
        | splitOkay && last f == '-' = (`Range` infinity) <$> first
        | splitOkay                  = checkOrdering $ Range <$> first <*> second
        | otherwise                  = Exact <$> num
    where
        -- try taking the input as a whole
        num = checkPositive $ readMaybe f

        -- try breaking it up
        parts     = splitOn "-" f
        first     = checkPositive $ readMaybe (head parts)
        second    = checkPositive $ readMaybe (last parts)
        splitOkay = length parts == 2

        checkPositive Nothing  = Nothing
        checkPositive (Just n)
            | n > 0     = Just n
            | otherwise = Nothing

        checkOrdering j@(Just (Range a b))
            | a < b     = j
            | otherwise = Nothing
        checkOrdering _ = Nothing


infinity :: Double
-- infinity is a nice alternative to infinite lists for our fields
infinity = read "Infinity"
