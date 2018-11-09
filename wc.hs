module Main where

-- wc, word count
--
--  -l lines
--  -w words
--  -c chars

import           Data.Maybe         (fromMaybe)
import           System.Directory   (doesFileExist)
import           System.Environment (getArgs)
import           System.Exit        (die)

data Flag = Lines
          | Words
          | Chars
          deriving (Show)

nlines c = show $ length (lines c)
nchars c = show $ length c
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
      output Nothing  = do
        stdinContent <- getContents
        putStrLn $ wc flags stdinContent

      output (Just c) = putStrLn $ wc flags c


main :: IO ()
main = do
  args <- getArgs
  handle [] args Nothing
