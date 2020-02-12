module Coreutils.Cat where

-- cat
--
-- read files from the command line or echo stdin

import           Coreutils.Util
import qualified Data.ByteString.Lazy as L

data Cat = Cat

instance Util Cat where
    run _ = runner

runner :: [String] -> IO ()
runner args
        | null files = L.getContents >>= L.putStr
        | otherwise  = mapM_ cat files
    where
        files = filter (/= "--") args
        cat "-" = L.getContents >>= L.putStr
        cat fn  = L.readFile fn >>= L.putStr
