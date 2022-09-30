module Coreutils.Ls where

import           Control.Exception
import           Control.Monad
import           Coreutils.Util
import           Data.List
import           Data.Time.Clock
import           System.Console.GetOpt
import           System.Directory
import           System.Exit
import           System.IO

data Ls = Ls

instance Util Ls where
    run Ls = lsMain

-- | IO

lsMain :: [String] -> IO ()
lsMain args = do
        unless (null errors) $
            die $ unlines errors
        either die (`runLs` other) $
            foldM (flip id) defaultOptions opts
    where
        (opts, other, errors) = getOpt RequireOrder optionDesc args

runLs :: Options -> [FilePath] -> IO ()
runLs os [] = runLs os ["."]
runLs os ps =
        execute rt ps >>= mapM_ putStrLn
    where
        rt = getRuntime os

execute :: Runtime -> [FilePath] -> IO [String]
execute rt ps = do
        fs <- concat <$> mapM list' ps
        map format' . sorter rt <$> mapM fill' fs
    where
        list' = list rt
        fill' = fill rt
        format' = format rt

-- | Runtime

data Entry = Entry {
      name  :: String
    , size  :: Maybe Integer
    , mtime :: Maybe UTCTime
    , atime :: Maybe UTCTime
    , perms :: Maybe Permissions
}
    deriving (Eq, Show)

getEntry :: Options -> FilePath -> IO Entry
getEntry os path = do
        size <- getSize
        (mtime, atime) <- getStat
        perms <- getPerms
        pure $ Entry { .. }
    where
        name = path

        getSize
            | optSortStrategy os == SizeSorting = do
                handle orZero $ Just <$> withFile path ReadMode hFileSize
            | otherwise = pure Nothing
        orZero :: IOException -> IO (Maybe Integer)
        orZero _ = pure $ Just 0

        getStat
            | optSortStrategy os == StatSorting = do
                mt <- getModificationTime path
                at <- getAccessTime path
                pure (Just mt, Just at)
            | otherwise = pure (Nothing, Nothing)

        getPerms = pure Nothing


data Runtime = Runtime {
      list   :: FilePath -> IO [FilePath]
    , fill   :: FilePath -> IO Entry
    , sorter :: [Entry] -> [Entry]
    , format :: Entry -> String
}

getRuntime :: Options -> Runtime
getRuntime os = Runtime { .. }
    where
        list path
            | optAllDots os = getDirectoryContents path
            | optHidden  os = listDirectory path
            | otherwise =
                filter (not . isDot) <$> listDirectory path

        fill = getEntry os

        sorter
            | optSort os = sortBy by
            | otherwise  = id
        by a b = reverse' $ case optSortStrategy os of
             Lexigraphic -> compare (name a) (name b)
             SizeSorting -> invert $ compare (size a) (size b)
             StatSorting -> invert $ compare (stat a) (stat b)
        stat e
            | optSortStat os == LastAccess = atime e
            | otherwise                    = mtime e
        reverse'
            | optSortReverse os = invert
            | otherwise         = id

        format = name

-- | Utility

isDot :: FilePath -> Bool
isDot ('.':_) = True
isDot _       = False

invert :: Ordering -> Ordering
invert LT = GT
invert EQ = EQ
invert GT = LT

-- | Options

data SortStrategy = Lexigraphic | SizeSorting | StatSorting
    deriving (Eq, Show)

data SortStat = LastModification | LastAccess
    deriving (Eq, Show)

data Options = Options {
      optSort         :: Bool
    , optSortStrategy :: SortStrategy
    , optSortReverse  :: Bool
    , optSortStat     :: SortStat
    , optAllDots      :: Bool
    , optHidden       :: Bool
}
    deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options {
      optSort = True
    , optSortStrategy = Lexigraphic
    , optSortReverse = False
    , optSortStat = LastModification
    , optAllDots = False
    , optHidden = False
}

optionDesc :: [OptDescr (Options -> Either String Options)]
optionDesc =
    [ Option "f" []
        (NoArg
            (\opt -> Right opt { optSort = False }))
        "Output is not sorted"

    , Option "r" []
        (NoArg
            (\opt -> Right opt { optSortReverse = True }))
        "Sort order is reversed"

    , Option "t" []
        (NoArg
            (\opt -> Right opt { optSortStrategy = StatSorting }))
        "Sort by descending time modified, newest first"

    , Option "S" []
        (NoArg
            (\opt -> Right opt { optSortStrategy = SizeSorting }))
        "Sort by size, largest first"

    , Option "u" []
        (NoArg
            (\opt -> Right opt { optSortStat = LastAccess }))
        "Consider access time instead of modification time"

    , Option "a" []
        (NoArg
            (\opt -> Right opt { optAllDots = True }))
        "Include directory entries that start with '.'"

    , Option "A" []
        (NoArg
            (\opt -> Right opt { optHidden = True }))
        "Include '.' and '..'"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "ls" optionDesc))
        "Show this help text"
    ]
