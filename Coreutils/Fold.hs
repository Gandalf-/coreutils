module Coreutils.Fold where

import           Coreutils.Util
import           Data.Char
import           System.Console.GetOpt
import           System.Exit           (exitFailure)

data Fold = Fold

instance Util Fold where
    run _ = foldMain

foldMain :: [String] -> IO ()
foldMain args = case getOpt Permute options args of
    (o, n, []) -> do
        let opts = foldl (>>=) (Right defaultOptions) o
        case opts of
            Left err -> do
                putStrLn err
                exitFailure
            Right os -> mapM_ (processFile os) n
    (_, _, errs) -> do
        mapM_ putStrLn errs
        exitFailure

processFile :: Options -> FilePath -> IO ()
processFile opts file = readFile file >>= putStr . foldContent opts

foldContent :: Options -> String -> String
foldContent opts = unlines . concatMap (foldLine opts) . lines

foldLine :: Options -> String -> [String]
foldLine opts = go 0 ""
  where
    width = optWidth opts
    go n acc xs
      | n >= width =
        let (pre, suf) = splitAtSpace acc
        in pre : go 0 "" (suf ++ xs)
      | otherwise =
        case xs of
            [] -> [acc]
            (c:cs) ->
                let p = positions (optPosition opts) c
                    n' = n + p
                in if n' > width && optPretty opts && isSpace c
                    then acc : go p [c] cs
                    else go n' (acc ++ [c]) cs


splitAtSpace :: String -> (String, String)
splitAtSpace s = case break isSpace (reverse s) of
    (a, b) -> (reverse b, reverse a)


spaceSplit :: String -> Int -> (String, String)
spaceSplit xs width = go xs "" 0
  where
    go :: String -> String -> Int -> (String, String)
    go [] acc _ = (acc, "")
    go (c:cs) acc len
      | isSpace c =
          let nextWord = takeWhile (not . isSpace) cs
              newLen = len + length nextWord + 1
          in if newLen > width
             then (acc, dropWhile isSpace cs)
             else go cs (acc ++ [c] ++ nextWord) newLen
      | otherwise = go cs (acc ++ [c]) (len + 1)


splitter :: (Char -> Int) -> String -> Int -> (String, String)
splitter widthFn xs maxWidth = go xs "" 0
  where
    go :: String -> String -> Int -> (String, String)
    go [] acc _ = (acc, "")
    go (c:cs) acc currentWidth
      | isSpace c =
          let (nextWord, rest) = break isSpace cs
              nextWordWidth = sum (map widthFn nextWord)
              spaceWidth = widthFn c
              newWidth = currentWidth + spaceWidth + nextWordWidth
          in if newWidth > maxWidth
             then (acc, cs)
             else go rest (acc ++ [c] ++ nextWord) newWidth
      | otherwise =
          let charWidth = widthFn c
          in go cs (acc ++ [c]) (currentWidth + charWidth)


data Position = Bytes | Columns

positions :: Position -> Char -> Int
positions Bytes _      = 1
positions Columns '\t' = 8
positions Columns '\r' = -1
positions Columns '\b' = -1
positions Columns _    = 1

-- | Options

data Options = Options
    { optWidth    :: Int
    , optPretty   :: Bool
    , optPosition :: Position
    }

defaultOptions :: Options
defaultOptions = Options
    { optWidth = 80
    , optPretty = False
    , optPosition = Columns
    }

parseWidth :: String -> Either String Int
parseWidth xs
    | all isDigit xs = Right $ read xs
    | otherwise = Left $ xs <> " is not a valid width"

options :: [OptDescr (Options -> Either String Options)]
options =
    [ Option "b" []
        (NoArg
            (\opt -> Right opt { optPosition = Bytes }))
        "Count width in bytes instead of column positions"

    , Option "s" []
        (NoArg
            (\opt -> Right opt { optPretty = True }))
        "Break lines on spaces when possible"

    , Option "w" []
        (ReqArg
            (\arg opt -> (\w -> opt { optWidth = w }) <$> parseWidth arg)
            "width")
        "Line width to use instead of the default of 80"
    ]
