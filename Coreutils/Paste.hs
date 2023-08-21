{-# LANGUAGE OverloadedStrings #-}

module Coreutils.Paste where

import           Control.Monad
import           Coreutils.Util
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import           System.Console.GetOpt
import           System.Exit
import           System.IO

data Paste = Paste

instance Util Paste where
    run _ = pasteMain

-- | IO

pasteMain :: [String] -> IO ()
pasteMain args = do
        unless (null errors) $
            die $ unlines errors

        when (null files) $
            die help

        either die (`runPaste` files) $
            foldM (flip id) defaultOptions opts
    where
        (opts, files, errors) = getOpt RequireOrder optionDesc args

runPaste :: Options -> [String] -> IO ()
runPaste (Options ds False) fs = do
    hs <- openHandles fs
    paste hs (cycle ds) >>= mapM_ B.putStrLn

runPaste (Options ds True) fs = do
    hs <- openHandles fs
    forM_ hs $ \h -> do
        serialize h (cycle ds) >>= mapM_ B.putStr

-- | Implementation

openHandles :: [String] -> IO [Handle]
openHandles = mapM opener
    where
        opener "-" = return stdin
        opener x   = openFile x ReadMode

getLines :: [Handle] -> IO [Maybe ByteString]
getLines = mapM reader
    where
        reader h = do
            done <- hIsEOF h
            if done
                then return Nothing
                else Just <$> B.hGetLine h

paste :: [Handle] -> [ByteString] -> IO [ByteString]
paste hs ds = do
    ls <- getLines hs
    if all isNothing ls
        then return []
        else do
            let out = delimit (map (fromMaybe B.empty) ls) ds
            next <- paste hs ds
            return $ out : next

delimit :: [ByteString] -> [ByteString] -> ByteString
delimit []  _           = B.empty
delimit _   []          = undefined
delimit [a] _           = a
delimit (a:b:bs) (d:ds) = a <> d <> delimit (b:bs) ds

serialize :: Handle -> [ByteString] -> IO [ByteString]
serialize h ds = do
    ls <- B.lines <$> B.hGetContents h
    pure $ combine ls ds

combine :: [ByteString] -> [ByteString] -> [ByteString]
combine [] _          = []
combine _ []          = undefined
combine [l] _         = [l <> "\n"]
combine (l:ls) (d:ds) = l <> d : combine ls ds

-- | Options

data Options = Options
    { optDelimiters :: [ByteString]
    , optSerial     :: Bool
    }

defaultOptions :: Options
defaultOptions = Options
    { optDelimiters = ["\t"]
    , optSerial     = False
    }

separators :: String -> [String]
separators ('\\':x:xs) =
    case x of
        't'  -> "\t" : separators xs
        'n'  -> "\n" : separators xs
        '\\' -> "\\" : separators xs
        '0'  -> ""   : separators xs
        _    -> [x]  : separators xs
separators (x:xs) = [x] : separators xs
separators []     = []

help :: String
help = usageInfo "paste [-s] [-d list] file ..." optionDesc

optionDesc :: [OptDescr (Options -> Either String Options)]
optionDesc =
    [ Option "d" []
        (ReqArg (\arg o -> Right o { optDelimiters = map B.pack $ separators arg }) "list")
        "Cyclically use characters from the list instead of TABs"

    , Option "s" []
        (NoArg (\o -> Right o { optSerial = True }))
        "Concatenate all lines of input files in command line order"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left help))
        "Show this help text"
    ]
