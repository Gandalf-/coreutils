module Coreutils.Sh where

-- sh
--
-- POSIX compliant shell, functional as a login shell if you bootstrap the
-- environment manually. Doesn't require any other shell to be present on the system.

import System.Process

import Coreutils.Util
import System.Directory
import GHC.IO.Handle
import System.IO

data Sh = Sh

instance Util Sh where
    run _ _ = undefined


data ShState = ShState {
      shCwd :: FilePath
    , shOldCwd :: FilePath
    , shEnv :: [(String, String)]
    , shStdin :: Handle
    , shStdout :: Handle
    , shStderr :: Handle
}

execute :: ShState -> FilePath -> [String] -> CreateProcess
execute st exe opts = (proc exe opts) {
          cwd = Just $ shCwd st
        , env = Just $ shEnv st
        , std_in = UseHandle $ shStdin st
        , std_out = UseHandle $ shStdout st
        , std_err = UseHandle $ shStderr st
    }


-- | Builtins

tryBuiltin :: ShState -> String -> [String] -> Maybe (IO ShState)
tryBuiltin st "cd" opts = Just $ builtin Cd st opts
tryBuiltin _ _ _ = Nothing

class Builtin a where
    builtin :: a -> ShState -> [String] -> IO ShState

data Cd = Cd

instance Builtin Cd where
    builtin _ st ["-"] = builtin Cd st [shOldCwd st]
    builtin _ st [p] =
        setCurrentDirectory p
        >> pure st { shCwd = p, shOldCwd = shCwd st }
    builtin _ st _   =
        hPutStrLn (shStderr st) "usage: [path]"
        >> pure st
