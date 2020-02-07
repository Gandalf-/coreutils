{-# LANGUAGE BangPatterns #-}
module Coreutils.Wc where

-- wc, word count
--
--  -l lines
--  -w words
--  -c chars

import           Data.Char        (isSpace)
import           Data.Maybe       (fromMaybe)
import           System.Directory (doesFileExist)
import           System.Exit      (die)

import           Coreutils.Util


data State = State
        { charCount :: !Int
        , lineCount :: !Int
        , wordCount :: !Int
        , prevSpace :: !Bool
        }
    deriving (Show, Eq)


wc' :: String -> State
wc' = foldr counter start

start :: State
start = State {
          charCount = 0
        , lineCount = 0
        , wordCount = 0
        , prevSpace = False
    }

counter :: Char -> State -> State
counter c s =
        State {
              charCount = charCount'
            , lineCount = lineCount'
            , wordCount = wordCount'
            , prevSpace = space
        }
    where
        !charCount' = charCount s + 1

        !lineCount'
            | c == '\n' = lineCount s + 1
            | otherwise = lineCount s

        !wordCount'
            | wordCount s == 0 && not space = 1
            | not space && prevSpace s      = wordCount s + 1
            | otherwise                     = wordCount s

        !space = isSpace c


data Flag = Lines
        | Words
        | Chars
        deriving (Show)


nlines :: String -> String
nlines c = show $ length (lines c)

nchars :: String -> String
nchars c = show $ length c

nwords :: String -> String
nwords c = show $ length $ words c


wc :: [Flag] -> String -> String
wc flags content
        | null flags = out content [Lines, Words, Chars]
        | otherwise  = out content flags
  where
        out :: String -> [Flag] -> String
        out c (Lines : fs) = nlines c ++ " " ++ out c fs
        out c (Words : fs) = nwords c ++ " " ++ out c fs
        out c (Chars : fs) = nchars c ++ " " ++ out c fs
        out _ []           = []


help :: IO ()
help = do
      putStrLn "usage: wc [option]"
      putStrLn ""
      putStrLn "  -l  lines"
      putStrLn "  -w  words"
      putStrLn "  -c  characters"


handle :: [Flag] -> [String] -> Maybe String -> IO ()
handle flags arguments content =
      case arguments of
        ("-h" : _   ) -> help
        ("-l" : args) -> handle (Lines:flags) args content
        ("-w" : args) -> handle (Words:flags) args content
        ("-c" : args) -> handle (Chars:flags) args content

        (file : args) -> do
          exists <- doesFileExist file
          if exists
            then do
              fileContent <- readFile file
              handle flags args $ Just (fileContent ++ fromMaybe "" content)

            else die $
              "wc: " ++ file ++ ": No such file or directory"

        [] -> output content
  where
      output :: Maybe String -> IO ()
      output Nothing  = getContents >>= putStrLn . wc flags
      output (Just c) = putStrLn $ wc flags c

data Wc = Wc

instance Util Wc where
        run _ _ = wc' <$> getContents >>= print

        -- handle [] args Nothing
