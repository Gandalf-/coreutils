module Main where

-- rev
--
-- read lines from stdin, print them out reversed

main :: IO ()
main = interact rev
    where
        rev = unlines . map reverse . lines
