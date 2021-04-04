{-# LANGUAGE FlexibleContexts #-}

module Coreutils.Test where

import           Control.Exception (IOException, catch)
import           System.Directory
import           Text.Parsec

data Expr
        = Single Op
        | And Expr Expr
        | Or Expr Expr
        | Not Expr
        | Sub Expr
    deriving (Show, Eq)

data Op = IoOp IoOp | PureOp PureOp
    deriving (Show, Eq)

data PureOp
        = StrEqual String String
        | StrNotEqual String String
        | StrLengthZero String
        | StrLengthNotZero String
        | NumEqual Double Double
        | NumNotEqual Double Double
        | NumGt Double Double
        | NumGe Double Double
        | NumLt Double Double
        | NumLe Double Double
    deriving (Show, Eq)

data IoOp
        = FileDir FilePath
        | FileExists FilePath
        | FileRegular FilePath
        | FileSymbolic FilePath
        | FileSizeNotZero FilePath
        | FileReadable FilePath
        | FileWriteable FilePath
        | FileExecutable FilePath
    deriving (Show, Eq)


sWord :: Stream s m Char => String -> ParsecT s u m ()
sWord t = spaces >> string t >> spaces

word :: Parsec String () String
word = many1 (noneOf " ()")

number :: Parsec String () Double
-- allows 1.2.3, which will blow up read
-- doesn't allow negative numbers
number = read <$> many1 (oneOf ('.': ['0'..'9']))

double :: Stream s m Char =>
    ParsecT s u m a1 -> String -> (a1 -> a1 -> a2) -> ParsecT s u m a2
double tok sep op = try $ do
        l <- tok
        sWord sep
        op l <$> tok

single :: Stream s m Char =>
    ParsecT s u m a1 -> String -> (a1 -> a2) -> ParsecT s u m a2
single tok sep op = try $ do
        sWord sep
        op <$> tok

stringOps :: Parsec String () Op
stringOps = PureOp <$> (
            double word "!= " StrNotEqual
        <|> double word "= "  StrEqual
        <|> single word "-z " StrLengthZero
        <|> single word "-n " StrLengthNotZero)

numberOps :: Parsec String () Op
numberOps = PureOp <$> (
            double number "-eq " NumEqual
        <|> double number "-ne " NumNotEqual
        <|> double number "-gt " NumGt
        <|> double number "-ge " NumGe
        <|> double number "-lt " NumLt
        <|> double number "-le " NumLe)

fileOps :: Parsec String () Op
fileOps = IoOp <$> (
            single word "-d " FileDir
        <|> single word "-e " FileExists
        <|> single word "-f " FileRegular
        <|> single word "-L " FileSymbolic
        <|> single word "-s " FileSizeNotZero
        <|> single word "-r " FileReadable
        <|> single word "-w " FileWriteable
        <|> single word "-x " FileExecutable)

condition :: Parsec String () Op
condition = try stringOps <|> try numberOps <|> fileOps

expression :: Parsec String () Expr
expression =
        try andE
        <|> try orE
        <|> try nExpr
        <|> try pExpr
        <|> Single <$> condition
  where
        andE = do
            left <- Single <$> condition <|> nExpr <|> pExpr
            sWord "-a"
            And left <$> expression

        orE = do
            left <- Single <$> condition <|> nExpr <|> pExpr
            sWord "-o"
            Or left <$> expression

        pExpr = Sub <$> between (sWord "(") (sWord ")") expression
        nExpr = do
            sWord "! "
            Not <$> expression

executePureOp :: PureOp -> Bool
executePureOp (StrEqual a b)        = a == b
executePureOp (StrNotEqual a b)     = a /= b

executePureOp (StrLengthZero "")    = True
executePureOp (StrLengthZero _)     = False

executePureOp (StrLengthNotZero "") = False
executePureOp (StrLengthNotZero _)  = True

executePureOp (NumEqual a b)        = a == b
executePureOp (NumNotEqual a b)     = a /= b
executePureOp (NumGt a b)           = a > b
executePureOp (NumGe a b)           = a >= b
executePureOp (NumLt a b)           = a < b
executePureOp (NumLe a b)           = a <= b

orFalse :: IO Bool -> IO Bool
orFalse f = catch f false
    where
        false :: IOException -> IO Bool
        false _ = pure False

executeIoOp :: IoOp -> IO Bool
executeIoOp (FileDir a)         = doesDirectoryExist a
executeIoOp (FileExists a)      = doesFileExist a
executeIoOp (FileRegular a)     = doesFileExist a -- TODO something else on Unix
executeIoOp (FileSymbolic a)    = pathIsSymbolicLink a

executeIoOp (FileSizeNotZero a) = orFalse ((> 0) <$> getFileSize a)
executeIoOp (FileReadable a)    = orFalse (readable <$> getPermissions a)
executeIoOp (FileWriteable a)   = orFalse (writable <$> getPermissions a)
executeIoOp (FileExecutable a)  = orFalse (executable <$> getPermissions a)

executeOp :: Op -> IO Bool
executeOp (IoOp o)   = executeIoOp o
executeOp (PureOp o) = pure $ executePureOp o

execute :: Expr -> IO Bool
-- run the commands! the result is a boolean, the exit code
execute (Single o) = executeOp o
execute (And a b) = do
        ar <- execute a
        if ar then execute b else pure False
execute (Or a b) = do
        ar <- execute a
        if ar then pure True else execute b
execute (Not a) = not <$> execute a
execute (Sub a) = execute a

testParse :: String -> Either ParseError Expr
testParse = parse (expression <* eof) "test"

test :: String -> Either ParseError (IO Bool)
test s = execute <$> testParse s
