-- wc, word count
--
--  -l lines
--  -w words
--  -c chars

import System.Environment (getArgs)

data Flag = Lines
          | Words
          | Chars
          deriving (Show)


nlines c = show $ length (lines c)
nchars c = show $ length c
nwords c = show $ length $ words c


result :: String -> [Flag] -> String
result c flags
  | null flags = f c [Lines, Words, Chars]
  | otherwise  = f c flags
  where f c (Lines:xs) = nlines c ++ " " ++ f c xs
        f c (Words:xs) = nwords c ++ " " ++ f c xs
        f c (Chars:xs) = nchars c ++ " " ++ f c xs
        f c []         = []

help = do
  putStrLn "usage: wc [option]"
  putStrLn "  -l  lines"
  putStrLn "  -w  words"
  putStrLn "  -c  characters"

fromFile fname flags = do
  content <- readFile fname
  putStrLn $ result content flags ++ fname

fromStdin flags = do
  content <- getContents
  putStrLn $ result content flags


action :: [String] -> [Flag] -> IO ()
action args flags =
  case args of
    ("-h":_)  -> help
    ("-l":xs) -> action xs $ flags ++ [Lines]
    ("-w":xs) -> action xs $ flags ++ [Words]
    ("-c":xs) -> action xs $ flags ++ [Chars]
    (f:xs)    -> do
      fromFile f flags
      action xs flags
    []        -> return ()


main = do
  args <- getArgs
  case args of
    [] -> fromStdin []
    _  -> action args []
