{-# LANGUAGE BangPatterns #-}

module Coreutils.Cat where

-- cat
--
-- read files from the command line or echo stdin
-- soldiers on when some files do not exist, but reports failure at the end

import           Control.Exception    (IOException, try)
import           Control.Monad        (when)
import qualified Data.ByteString.Lazy as L
import           Data.Either          (isLeft)
import           System.Exit          (exitFailure)
import           System.IO            (hPrint, stderr)

import           Coreutils.Util

data Cat = Cat

instance Util Cat where
    run _ args = collect args >>= display

type Argument    = String
type FileContent = L.ByteString

collect :: [Argument] -> IO [Either IOException FileContent]
collect = mapM (try . L.readFile)

display :: [Either IOException FileContent] -> IO ()
display [] = L.getContents >>= L.putStr

display files = do
        let !failure = any isLeft files
        mapM_ toConsole files
        when failure exitFailure
    where
        toConsole (Left exception) = hPrint stderr exception
        toConsole (Right content)  = L.putStr content
