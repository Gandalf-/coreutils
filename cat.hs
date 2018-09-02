module Main where

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Data.Maybe (fromMaybe)

handle :: [String] -> Maybe String -> IO ()
handle arguments content =
  case arguments of

      (file : args) -> do
        exists <- doesFileExist file
        if exists
          then do
            fileContent <- readFile file
            handle args $ Just (fileContent ++ fromMaybe "" content)

          else putStrLn $ "cat: " ++ file ++ ": No such file or directory"

      [] -> output content

  where output :: Maybe String -> IO ()
        output Nothing  = do
          content <- getContents
          putStrLn content

        output (Just c) = putStrLn c

main = do
  args <- getArgs
  handle args Nothing
