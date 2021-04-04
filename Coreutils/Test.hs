{-# LANGUAGE FlexibleContexts #-}

module Coreutils.Test where

import           Text.Parsec

data Expr
        = Single Op
        | And Expr Expr
        | Or Expr Expr
        | Not Expr
        | Sub Expr
    deriving (Show, Eq)

data Op
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
        | FileDir FilePath
        | FileExists FilePath
        | FileRegular FilePath
        | FileSymbolic FilePath
        | FileReadable FilePath
        | FileSizeNotZero FilePath
        | FileWriteable FilePath
        | FileExecutable FilePath
    deriving (Show, Eq)

sWord :: Stream s m Char => String -> ParsecT s u m ()
sWord t = spaces >> string t >> spaces

word :: Parsec String () String
word = many1 (noneOf " ()")

number :: Parsec String () Double
-- allows 1.2.3, which will blow up read
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
stringOps =
        double word "!=" StrNotEqual
        <|> double word "=" StrEqual
        <|> single word "-z" StrLengthZero
        <|> single word "-n" StrLengthNotZero

numberOps :: Parsec String () Op
numberOps =
        double number "-eq" NumEqual
        <|> double number "-ne" NumNotEqual
        <|> double number "-gt" NumGt
        <|> double number "-ge" NumGe
        <|> double number "-lt" NumLt
        <|> double number "-le" NumLe

fileOps :: Parsec String () Op
fileOps =
        single word "-d" FileDir
        <|> single word "-e" FileExists
        <|> single word "-f" FileRegular
        <|> single word "-L" FileSymbolic
        <|> single word "-r" FileReadable
        <|> single word "-s" FileSizeNotZero
        <|> single word "-w" FileWriteable
        <|> single word "-x" FileExecutable

condition :: Parsec String () Op
condition = try stringOps <|> try numberOps <|> fileOps

expression :: Parsec String () Expr
expression =
        try andE
        <|> try orE
        <|> try pExpr
        <|> Single <$> condition
  where
        pExpr = Sub <$> between (sWord "(") (sWord ")") expression

        andE = do
            left <- Single <$> condition <|> pExpr
            sWord "-a"
            And left <$> expression

        orE = do
            left <- Single <$> condition <|> pExpr
            sWord "-o"
            Or left <$> expression

testParse :: String -> Either ParseError Expr
testParse = parse (expression <* eof) "test"
