{-# LANGUAGE OverloadedStrings #-}

module Coreutils.Awk where

import qualified Data.Text as T
import           Data.Text (Text)
import           Text.Parsec

import Coreutils.Util

data Awk = Awk

instance Util Awk where
    run _ = undefined

fields :: Text -> [Text]
fields = filter (not . T.null) . T.splitOn " "

awkParse :: Text -> Either ParseError Expr
awkParse _ = Right EmptyExpr

data Expr =
      EmptyExpr
    | ActionExpr Action
    deriving (Eq, Show)

pExpr :: Parsec Text () Expr
pExpr = choice [try action, pEmpty]
    where
        action = do
            spaces >> char '{' >> spaces
            a <- pAction
            spaces >> char '}' >> spaces
            pure $ ActionExpr a

pEmpty :: Parsec Text () Expr
pEmpty = choice [try emptyBrace, emptyString]
    where
        emptyString = spaces >> pure EmptyExpr
        emptyBrace = do
            spaces >> char '{' >> spaces >> char '}' >> spaces
            pure EmptyExpr

data Action =
      PrintAll
    | PrintValue [Value]
    deriving (Eq, Show)

pAction :: Parsec Text () Action
pAction = choice [try pVar, pAll]
    where
        pAll = do
            _ <- string "print"
            pure PrintAll
        pVar = do
            _ <- string "print"
            spaces
            vs <- sepEndBy1 pValue spaces
            pure $ PrintValue vs

data Value =
      String Text
    | FieldVar Int
    | Separator
    deriving (Eq, Show)

pValue :: Parsec Text () Value
pValue = choice [sep, str, field]
    where
        field = FieldVar <$> do
            _ <- char '$'
            read <$> many1 digit
        sep = do
            _ <- char ','
            pure Separator
        str = do
            _ <- char '"'
            s <- many1 (noneOf "\"")
            _ <- char '"'
            pure $ String $ T.pack s
