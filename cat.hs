module Main where

-- cat
--
-- read files from the command line or echo stdin

import           Data.Maybe         (fromMaybe)
import           System.Directory   (doesFileExist)
import           System.Environment (getArgs)


cat :: [String] -> Maybe String -> IO ()
cat args content =
      case args of
          (file : args) -> do
              exists <- doesFileExist file
              if exists
                then do
                  fileContent <- readFile file
                  cat args $ Just (fileContent ++ fromMaybe "" content)

                else putStrLn $
                  "cat: " ++ file ++ ": No such file or directory"

          [] -> output content
  where
      output :: Maybe String -> IO ()
      output Nothing  = do
        content <- getContents
        putStrLn content

      output (Just c) = putStrLn c


main :: IO ()
main = do
      args <- getArgs
      cat args Nothing
