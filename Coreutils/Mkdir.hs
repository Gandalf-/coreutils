module Coreutils.Mkdir where

-- mkdir
--
-- Create a directory and optionally set owner permissions via a mode

import           Control.Monad
import           Coreutils.Util
import           Data.Bits
import           Data.Word
import           System.Console.GetOpt
import           System.Directory
import           System.Exit

data Mkdir = Mkdir

instance Util Mkdir where
    run _ = mkdirMain

-- | IO

mkdirMain :: [String] -> IO ()
mkdirMain args = do
        unless (null errors) $
            die $ unlines errors
        either die (`runMkdir` other) $
            foldM (flip id) defaultOptions opts
    where
        (opts, other, errors) = getOpt RequireOrder optionDesc args

runMkdir :: Options -> [FilePath] -> IO ()
runMkdir _  [] = die usage
runMkdir os fs = either die (executes fs) (getRuntime os)
    where
        executes paths rt = mapM_ (execute rt) paths
        execute  rt path  = mkdir rt path >> chmod rt path

-- | Implementation

data Runtime = Runtime {
      mkdir :: FilePath -> IO ()
    , chmod :: FilePath -> IO ()
}

getRuntime :: Options -> Either String Runtime
getRuntime os = do
    pure Runtime {
          mkdir = doMkdir
        , chmod = doChmod
    }
    where
        doChmod = maybe doNothing (flip setPermissions) (optMode os)
        doNothing _ = pure ()
        doMkdir
            | optParents os = createDirectoryIfMissing True
            | otherwise     = createDirectory

type Mode = Word8

parsePerms :: Mode -> Permissions
parsePerms m = s $ r $ w emptyPermissions
    where
        s = setOwnerSearchable (testBit m 0)
        w = setOwnerWritable   (testBit m 1)
        r = setOwnerReadable   (testBit m 2)

parseMode :: String -> Either String (Mode, Mode, Mode)
parseMode xs@[u, g, o]
        | (not . all mode) xs = parseMode []
        | otherwise           = pure (read [u], read [g], read [o])
    where
        mode c = c `elem` ['0' .. '7']
parseMode xs = Left $ xs <> " is not a valid octal mode"

-- | Options

data Options = Options {
      optMode    :: Maybe Permissions
    , optParents :: Bool
}

defaultOptions :: Options
defaultOptions = Options {
      optMode = Nothing
    , optParents = False
}

usage :: String
usage = usageInfo "mkdir [options] directory_name" optionDesc

getUserPerms :: String -> Either String Permissions
getUserPerms xs = parsePerms . (\(u, _, _) -> u) <$> parseMode xs

optionDesc :: [OptDescr (Options -> Either String Options)]
optionDesc =
    [ Option "m" []
        (ReqArg
            (\arg opt -> (\v -> opt { optMode = Just v }) <$> getUserPerms arg)
            "MODE")
        "Set the permissions of the created directory; group and other are zero'd"

    , Option "p" []
        (NoArg
            (\opt -> Right opt { optParents = True }))
        "Create intermediate directories if necessary"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left usage))
        "Show this help text"
    ]
