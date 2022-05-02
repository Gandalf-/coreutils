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

class Comparator a where
    matches :: a -> Record -> Bool


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

instance Comparator Pattern where
    matches Always       _ = True
    matches (Regex p)    r = _line r =~ p
    matches (Not p)      r = not $ matches p r
    matches (And p1 p2)  r = matches p1 r && matches p2 r
    matches (Or  p1 p2)  r = matches p1 r || matches p2 r
    matches (Relation v) r = matches v r
    matches _            _ = False

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

instance Comparator Relation where
    matches (RelEqual a b) r = comp (==) r a b
    matches (RelNotEq a b) r = comp (/=) r a b
    matches (RelLt    a b) r = comp (<)  r a b
    matches (RelLe    a b) r = comp (<=) r a b
    matches (RelGt    a b) r = comp (>)  r a b
    matches (RelGe    a b) r = comp (>=) r a b

comp :: (Primitive -> Primitive -> Bool) -> Record -> Value -> Value -> Bool
comp c r v1 v2 = c a b
    where
        a = expand r v1
        b = expand r v2

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
    execute (PrintValue vs) r = T.concat $ map (T.pack . show . expand r) vs <> ["\n"]

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


data Primitive =
      String Text
    | Number Int

instance Show Primitive where
    show (String s) = T.unpack s
    show (Number n) = show n

instance Eq Primitive where
    (==) (String s1) (String s2) = s1 == s2
    (==) (Number n1) (Number n2) = n1 == n2
    (==) (String s1) (Number n2) = s1 == T.pack (show n2)
    (==) (Number n1) (String s2) = T.pack (show n1) == s2

instance Ord Primitive where
    (<=) (String s1) (String s2) = s1 <= s2
    (<=) (Number n1) (Number n2) = n1 <= n2
    (<=) (String s1) (Number n2) = s1 <= T.pack (show n2)
    (<=) (Number n1) (String s2) = T.pack (show n1) <= s2

pAny :: Parsec Text () Primitive
pAny = String . T.pack <$> many1 anyChar

pPrimitive :: Parsec Text () Primitive
pPrimitive = choice [try quote, num]
    where
        quote = do
            _ <- char '"'
            s <- many1 (noneOf "\"")
            _ <- char '"'
            pure $ String $ T.pack s
        num = Number . read <$> many1 digit


data Value =
      Primitive Primitive
    | FieldVar Int
    | NumFields
    deriving (Eq, Show)

expand :: Record -> Value -> Primitive
expand _ (Primitive p) = p
expand r NumFields     = Number $ length $ _fields r
expand r (FieldVar 0)  = String $ _line r
expand r (FieldVar n)
    | n <= length (_fields r) = String $ _fields r !! (n - 1)
    | otherwise               = String ""

pValue :: Parsec Text () Value
pValue = choice [try sep, try field, try nf, pr]
    where
        pr = Primitive <$> pPrimitive
        nf = NumFields <$ string "NF"

        field = FieldVar <$> do
            _ <- char '$'
            read <$> many1 digit
        sep = do
            _ <- char ','
            pure $ Primitive $ String " "
