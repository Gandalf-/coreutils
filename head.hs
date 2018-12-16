module Main where

-- head
--
-- show some number of lines or characters from the top or bottom of files
-- or stdin

import           Control.Exception  (IOException, try)
import           System.Environment (getArgs)
import           System.Exit
import           Text.Read          (readMaybe)


data Argument = FlagLines Int
              | FlagChars Int
              | ParseError String
              | File String
        deriving (Show)

type HeadFunction = (String -> String)


main :: IO ()
-- ^ parse command line arguments, extract components, run
main = do
        args <- parse <$> getArgs

        let function = getHeadFunction args
            errors = getErrors args
            files = getFiles args

        if null errors
            then mapM_ (runHeadFunction function) files
            else mapM_ die errors


runHeadFunction :: HeadFunction -> FilePath -> IO ()
-- ^ get the content of the file, and apply the head function to it
runHeadFunction result path = do
       errorOrFile <- collectFile path

       case errorOrFile of
           Left _        -> die $ "Unable to read file " ++ path
           Right content -> putStr $ result content


collectFile :: FilePath -> IO (Either IOException String)
-- ^ read a file or stdin, account for possibility of exceptions
collectFile "-"  = Right <$> getContents
collectFile path = (try . readFile) path


getErrors :: [Argument] -> [String]
-- ^ extract parse errors from the arguments
getErrors args = [e | ParseError e <- args ]


getFiles :: [Argument] -> [FilePath]
-- ^ extract file paths from the arguments, if there aren't any use stdin
getFiles args = if null files then ["-"] else files
    where
        files = [f | File f <- args]


type Lines = Maybe Int
type Chars = Maybe Int

getHeadFunction :: [Argument] -> HeadFunction
getHeadFunction args = choose linesFlag charsFlag
    where
        -- get the first of each type, may not exist
        linesFlag = first [l | FlagLines l <- args]
        charsFlag = first [c | FlagChars c <- args]

        first []    = Nothing
        first (a:_) = Just a

        choose :: Lines -> Chars -> HeadFunction
        -- default, 10 lines
        choose Nothing Nothing = unlines . take 10 . lines

        -- lines from the top or bottom
        choose (Just l) _
            | l >= 0    = unlines . take l . lines
            | otherwise = unlines . take (negate l) . reverse . lines

        -- characters from the top or bottom
        choose _ (Just c)
            | c >= 0    = take c
            | otherwise = take (negate c) . reverse


parseFlag :: (Int -> Argument) -> String -> Argument
parseFlag f argument =
            case parsed of
                Just number -> f number
                _           -> ParseError $ message argument
        where
            parsed = readMaybe argument :: Maybe Int
            message m = unwords ["Unable to use", m, "as a number"]


parse :: [String] -> [Argument]
parse []                   = []
parse ("-n" : x : xs)      = parseFlag FlagLines x : parse xs
parse ("-c" : x : xs)      = parseFlag FlagChars x : parse xs
parse ("--lines" : x : xs) = parseFlag FlagLines x : parse xs
parse ("--bytes" : x : xs) = parseFlag FlagChars x : parse xs
parse (x : xs)             = File x : parse xs
