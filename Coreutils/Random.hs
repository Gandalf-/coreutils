module Coreutils.Random where

-- Choose random values within numeric bounds or from an argument list
-- From the fish shell

import           Coreutils.Util
import           Data.Char
import           System.Exit
import           System.Random  (StdGen, getStdGen, mkStdGen, randomR)

data Random = Random

instance Util Random where
    run _ = either die execute . options

-- | IO

execute :: Runtime -> IO ()
execute rt = getGen rt >>= (putStrLn . result rt)

-- | Implementation

data Bounds = Bounds {
      _low  :: Integer
    , _step :: Integer
    , _high :: Integer
}
    deriving (Show)

random :: Bounds -> StdGen -> Integer
random (Bounds low step h) g =
        low + step * fst (randomR (0, high) g)
    where
        high = (h - low) `div` step

choice :: [String] -> StdGen -> String
choice xs g = xs !! i
    where
        i = fst (randomR (0, length xs - 1) g)

-- | Options

data Runtime = Runtime {
      getGen :: IO StdGen
    , result :: StdGen -> String
}

parse :: String -> Either String Integer
parse s
    | all isDigit s = Right $ read s
    | otherwise     = Left $ s <> " is not a number"

defaultBounds :: Bounds
defaultBounds = Bounds 0 1 32767

options :: [String] -> Either String Runtime
options [] = Right $ Runtime getStdGen $ show . random defaultBounds

options ("choice":xs)
    | null xs   = Left "the ITEM list may not be empty"
    | otherwise = Right $ Runtime getStdGen (choice xs)

options [s] = do
    seed <- fromIntegral <$> parse s
    Right $ Runtime (pure $ mkStdGen seed) $ show . random defaultBounds

options [ls, hs] = do
    l <- parse ls
    h <- parse hs
    Right $ Runtime getStdGen (show . random (Bounds l 1 h))

options [ls, ss, hs] = do
    l <- parse ls
    s <- parse ss
    h <- parse hs
    Right $ Runtime getStdGen (show . random (Bounds l s h))

options _ = Left $ unlines
    [ "random"
    , "random SEED"
    , "random START END"
    , "random START STEP END"
    , "random choice [ITEMS...]"
    ]
