module Coreutils.Ed where

data Mode = Command | Input


data State = State
    { buffer   :: [String]
    , position :: Int
    , err      :: String
    }
    deriving (Show, Eq)


execute :: State -> String -> (State, String)
execute s "p" = printLine s
execute _ _   = undefined

-- | Commands

printLine :: State -> (State, String)
printLine s
    | invalid   = failure s "invalid address"
    | otherwise = success s (buffer s !! index)
  where
    invalid = index < 0 || index >= length (buffer s)
    index = position s - 1

changePosition :: State -> String -> (State, String)
changePosition s p
    | isNumber p && valid = success s { position = position } ""
    | otherwise  = failure s "invalid address"
  where
    position = read p :: Int
    valid = position > 0 && position <= length (buffer s)

-- | Addresses

data Address = Position Int | Range Int Int
    deriving (Show, Eq)

address :: State -> String -> Either String Address
address s xs = do
    addr <- parseAddr s xs
    validateAddress s addr

parseAddr :: State -> String -> Either String Address
parseAddr s "."      = Right $ Position $ position s
parseAddr s "$"      = Right $ Position $ length (buffer s)
parseAddr s "-"      = Right $ Position $ position s - 1
parseAddr s "^"      = Right $ Position $ position s - 1
parseAddr s "+"      = Right $ Position $ position s + 1
parseAddr s ","      = Right $ Range 1 $ length (buffer s)
parseAddr s "%"      = Right $ Range 1 $ length (buffer s)
parseAddr s ";"      = Right $ Range (position s) $ length (buffer s)
parseAddr s ('-':xs) = toNumber xs >>= \n -> Right $ Position $ position s - n
parseAddr s ('^':xs) = toNumber xs >>= \n -> Right $ Position $ position s - n
parseAddr _ _        = Left "invalid address"

-- | Utility

toNumber :: String -> Either String Int
toNumber s
    | isNumber s = Right $ read s
    | otherwise  = Left "invalid address"

validateAddress :: State -> Address -> Either String Address
validateAddress s (Position p)
    | p > 0 && p <= length (buffer s) = Right $ Position p
    | otherwise = Left "invalid address"
validateAddress s (Range from to)
    | from > 0 && to <= length (buffer s) && from <= to = Right $ Range from to
    | otherwise = Left "invalid address"

isNumber :: String -> Bool
isNumber "" = False
isNumber xs = all (`elem` ['0' .. '9']) xs

failure :: State -> String -> (State, String)
failure s msg = (s { err = msg }, "?")

success :: State -> String -> (State, String)
success s msg = (s { err = "" }, msg)
