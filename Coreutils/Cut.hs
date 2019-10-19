module Coreutils.Cut where

import           Data.List
import           Data.List.Split (splitOn)
import           Data.Maybe
import           System.Exit     (die)
import           System.IO
import           Text.Read       (readMaybe)

import           Coreutils.Util


-- | Data types

data Field = Field Double Double
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


-- | Business logic

cut :: [Field] -> [a] -> [a]
-- ^ an element is accepted if its index falls within the range of any of the fields
-- provided. all other elements are discarded
cut fields elements =
        map fst . filter anyInbounds $ zip elements [1..]
    where
        anyInbounds element =
            any (\ field -> inbounds field $ snd element) fields


cutComplement :: [Field] -> [a] -> [a]
-- ^ an element is rejected if its index falls within the range of any of the fields
-- provided. all other elements are kept
cutComplement fs xs =
        map fst . filter allOutbounds $ zip xs [1..]
    where
        allOutbounds x = all (\f -> not . inbounds f $ snd x) fs


inbounds :: Field -> Double -> Bool
inbounds (Field fmin fmax) x = x >= fmin && x <= fmax


-- | IO

data Cut = Cut

instance Util Cut where
    -- ^ library hook
    run _ args =
        case parseOptions defaultOptions args of
            Left e  -> die e
            Right o -> cutMain o

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
                (lines <$> hGetContents h)
                >>= mapM_ (filterPrint . runFields field)

            (Bytes _) ->
                die "not implemented"

            _ ->
                die "exactly one of: bytes, characters, or fields must be provided"
    where
        runFields f =
            intercalate (fromMaybe [delimiter o] $ outputDelimiter o)
            . cutFunction f
            . splitOn [delimiter o]

        runChars f =
            intercalate (fromMaybe "" $ outputDelimiter o)
            . groupBy (const . const False)
            . cutFunction f

        filterPrint s
            | onlyDelimited o =
                if delimiter o `elem` s
                    then putStrLn s
                    else pure ()
            | otherwise = putStrLn s

        cutFunction
            | complement o = cutComplement
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
        -- ^ treat these unknown things as input files
        parseOptions (o { inputs = x : inputs o}) xs

parseOptions base [] = Right base


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
        | splitOkay && head f == '-' = Field 1 <$> second
        | splitOkay && last f == '-' = (`Field` infinity) <$> first
        | splitOkay                  = Field <$> first <*> second
        | otherwise                  = (\ c -> Field c c) <$> num
    where
        -- try taking the input as a whole
        num = readMaybe f

        -- try breaking it up
        parts     = splitOn "-" f
        first     = readMaybe (head parts)
        second    = readMaybe (last parts)
        splitOkay = length parts == 2

        -- infinity is a cheap alternative to infinite lists
        infinity = read "Infinity" :: Double
