module Main where

-- head
--
-- show some number of lines or characters from the top or bottom of files
-- or stdin

import           Control.Monad
import           System.Console.GetOpt
import           System.Environment
import           System.Exit

data Options = Options  { optQuiet :: Bool
                        , optLines :: Int
                        , optChars :: Maybe Int
                        }

startOptions :: Options
startOptions = Options  { optQuiet = False
                        , optLines = 10
                        , optChars = Nothing
                        }

options :: [ OptDescr (Options -> Either String Options) ]
options =
    [ Option "l" ["lines"]
        (ReqArg
            (\arg opt -> case reads arg of
              [(nlines, "")] -> Right opt { optLines = nlines }
              _              -> Left $ "parse error on " <> arg)
            "LINES")
        "Number of lines"

    , Option "c" ["chars"]
        (ReqArg
            (\arg opt -> case reads arg of
              [(nchars, "")] -> Right opt { optChars = Just nchars }
              _              -> Left $ "parse error on " <> arg)
            "LINES")
        "Number of characters"

    , Option "q" ["quiet", "silent"]
        (NoArg
            (\opt -> Right opt { optQuiet = True }))
        "Do not show headers for files"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "head" options))
        "Show help"
    ]

type HeadF = FilePath -> IO ()

charsOp :: Int -> String -> String
charsOp n content
    | n >= 0    = take n content
    | otherwise = drop (length content - (negate n)) content

stdinCharHead :: Int -> IO ()
stdinCharHead n = interact (charsOp n)

charHead :: Int -> FilePath -> IO ()
charHead n f = charsOp n <$> readFile f >>= putStr


linesOp :: Int -> String -> String
linesOp n
    | n >= 0    = unlines . take n . lines
    | otherwise = unlines . take (negate n) . reverse . lines

stdinLineHead :: Int -> IO ()
stdinLineHead n = interact (linesOp n)

lineHead :: Int -> FilePath -> IO ()
lineHead n f = linesOp n <$> readFile f >>= putStr


switch :: (HeadF -> FilePath -> IO ()) -> Int -> Maybe Int -> [FilePath] -> IO ()
switch _ _ (Just c) []  = stdinCharHead c
switch _ n _ []         = stdinLineHead n

switch _ _ (Just c) [f] = charHead c f
switch _ n _ [f]        = lineHead n f

switch f _ (Just c) fs  = mapM_ (f (charHead c)) fs
switch f n _ fs         = mapM_ (f (lineHead n)) fs

header :: HeadF -> FilePath -> IO ()
header f s = do
    putStrLn $ "==> " <> s <> " <==="
    f s
    putStrLn ""


main :: IO ()
main = do
    args <- getArgs
    let (actions, files, errors) = getOpt RequireOrder options args

    when (not . null $ errors) $
        die $ show errors

    case foldM (flip id) startOptions actions of
        Left   err -> die err
        Right opts -> do
            let Options { optChars = nChars
                        , optLines = nLines
                        , optQuiet = quiet
                        } = opts

            let headerFunc = if quiet
                                 then id
                                 else header
            switch headerFunc nLines nChars files
