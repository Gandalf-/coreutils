module Main where

-- rev
--
-- read lines from stdin, print them out reversed

main :: IO ()
main = rev <$> getContents >>= putStr
    where
        rev = unlines . map reverse . lines
