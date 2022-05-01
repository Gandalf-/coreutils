{-# LANGUAGE OverloadedStrings #-}

module Coreutils.Awk where

import qualified Data.Text as T
import           Data.Text (Text)
import           Text.Regex.TDFA
import           Text.Parsec

import Coreutils.Util

data Awk = Awk

instance Util Awk where
    run _ = undefined


data Record = Record
    { _line :: Text
    , _fields :: [Text]
    }
    deriving (Eq, Show)

fields :: Text -> [Text]
fields = filter (not . T.null) . T.splitOn " "

getRecord :: Text -> Record
getRecord t = Record t (fields t)

class Executor a where
    execute :: a -> Record -> Text


data Program =
      Full Pattern [Action]
    | Grep Pattern
    | Exec [Action]
    | NoProgram
    deriving (Eq, Show)

instance Executor Program where
    execute NoProgram  _ = ""
    execute (Grep p)   r = execute (Full p [PrintAll]) r
    execute (Exec a)   r = execute (Full Always a)   r
    execute (Full p as) r
        | matches p r = T.concat (map (`execute` r) as)
        | otherwise   = ""

pProgram :: Parsec Text () Program
pProgram = choice [try full, try exec, try grep, pEmpty]
    where
        grep = Grep <$> pPattern
        full = do
            p <- pPattern
            spaces
            Full p . _actions <$> pExpr
        exec = Exec . _actions <$> pExpr

pEmpty :: Parsec Text () Program
pEmpty = choice [try emptyBrace, emptyString]
    where
        emptyString = spaces >> pure NoProgram
        emptyBrace = do
            spaces >> char '{' >> spaces >> char '}' >> spaces
            pure NoProgram


data Pattern =
      Begin
    | End
    | Regex Text
    | Relation Relation
    | Not Pattern
    | And Pattern Pattern
    | Or Pattern Pattern
    | Always
    deriving (Eq, Show)

matches :: Pattern -> Record -> Bool
matches Always      _ = True
matches (Regex p)   r = _line r =~ p
matches (Not p)     r = not $ matches p r
matches (And p1 p2) r = matches p1 r && matches p2 r
matches (Or  p1 p2) r = matches p1 r || matches p2 r
matches _           _ = False

pPattern :: Parsec Text () Pattern
pPattern = choice [try pNot, try pCond, try regex, try rel, try begin, end]
    where
        begin = string "BEGIN" >> pure Begin
        end   = string "END"   >> pure End
        pCond = try pAnd <|> pOr
        pNorm = choice [try pNot, try pCond, try rel, regex]

        pAnd = do
            p1 <- choice [try regex, pNot]
            spaces >> string "&&" >> spaces
            And p1 <$> pNorm
        pOr = do
            p1 <- choice [try regex, pNot]
            spaces >> string "||" >> spaces
            Or p1 <$> pNorm
        pNot =
            char '!' >> spaces >> Not <$> pNorm

        rel = Relation <$> pRelation
        regex = do
            _ <- char '/'
            r <- many1 (noneOf "/")
            _ <- char '/'
            pure $ Regex $ T.pack r


data Relation =
      RelEqual Value Value
    | RelNotEq Value Value
    | RelLt    Value Value
    | RelLe    Value Value
    | RelGt    Value Value
    | RelGe    Value Value
    deriving (Eq, Show)

pRelation :: Parsec Text () Relation
pRelation = choice [try eq, try neq, try lt, try le, try gt, ge]
    where
        neq = go "!=" RelNotEq
        eq  = go "==" RelEqual
        lt  = go "<"  RelLt
        le  = go "<=" RelLe
        gt  = go ">"  RelGt
        ge  = go ">=" RelGe

        go s b = do
            v1 <- pValue
            spaces >> string s >> spaces
            b v1 <$> pValue


newtype Expr = ActionExpr
    { _actions :: [Action]
    }
    deriving (Eq, Show)

pExpr :: Parsec Text () Expr
pExpr = do
    spaces >> char '{' >> spaces
    as <- sepEndBy1 pAction spaces
    spaces >> char '}' >> spaces
    pure $ ActionExpr as


data Action =
      PrintAll
    | PrintValue [Value]
    deriving (Eq, Show)

instance Executor Action where
    execute PrintAll        r = _line r <> "\n"
    execute (PrintValue vs) r = T.concat $ map (expand r) vs <> ["\n"]

pAction :: Parsec Text () Action
pAction = do
        o <- choice [try pVar, pAll]
        optional (char ';')
        pure o
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
    | NumFields
    deriving (Eq, Show)

expand :: Record -> Value -> Text
expand _ (String s) = s
expand _ Separator  = " "
expand r NumFields  = T.pack $ show $ length $ _fields r

expand r (FieldVar 0) = _line r
expand r (FieldVar n)
    | n <= length (_fields r) = _fields r !! (n - 1)
    | otherwise = ""

pValue :: Parsec Text () Value
pValue = choice [sep, str, field, numFields]
    where
        numFields =
            NumFields <$ string "NF"
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
