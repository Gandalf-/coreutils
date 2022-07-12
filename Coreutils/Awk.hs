{-# LANGUAGE OverloadedStrings #-}

module Coreutils.Awk where

import           Control.Monad
import           Data.Either
import qualified Data.HashMap.Strict   as H
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import qualified Data.Text.Lazy        as L
import qualified Data.Text.Lazy.IO     as L
import           System.Console.GetOpt
import           System.Exit
import           Text.Parsec
import           Text.Parsec.Text      (Parser)
import           Text.Regex.TDFA

import           Coreutils.Util

data Awk = Awk

instance Util Awk where
    run _ = awkMain


data Record = Record
    { _line   :: Text
    , _fields :: [Text]
    }
    deriving (Eq, Show)

data AwkState = AwkState
    { sVariables :: H.HashMap Text Primitive
    , sRecords   :: Int
    , sSeparator :: Text
    }
    deriving (Eq, Show)

emptyState :: AwkState
emptyState = AwkState H.empty 0 T.empty

emptyRecord :: Record
emptyRecord = Record T.empty []

fields :: Text -> [Text]
fields = filter (not . T.null) . T.splitOn " "

getRecord :: Text -> Record
getRecord t = Record t (fields t)

class Executor a where
    execute :: a -> AwkState -> Record -> (AwkState, Text)

data FullProgram = FullProgram
    { fBegin  :: [Program]
    , sMiddle :: [Program]
    , fEnd    :: [Program]
    }
    deriving (Eq, Show)

instance Executor FullProgram where
    execute (FullProgram b m e) = execute (b <> m <> e)

data Program =
      Full Pattern [Action]
    | Grep Pattern
    | Exec [Action]
    | NoProgram
    deriving (Eq, Show)

instance Executor Program where
    execute NoProgram   st _ = (st, "")
    execute (Grep p)    st r = execute (Full p [PrintAll]) st r
    execute (Exec a)    st r = execute (Full Always a)     st r
    execute (Full p as) st r
        | matches p st r = execute as st r
        | otherwise   = (st, "")

instance Executor a => Executor [a] where
    execute []     st _ = (st, "")
    execute [a]    st r = execute a st r
    execute (a:as) st r = (nextSt, thisText <> nextText)
        where
            prevSt = st
            (thisSt, thisText) = execute a prevSt r
            (nextSt, nextText) = execute as thisSt r


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

class Comparator a where
    matches :: a -> AwkState -> Record -> Bool

instance Comparator Pattern where
    matches Always       _  _ = True
    matches (Regex p)    _  r = _line r =~ p
    matches (Not p)      st r = not $ matches p st r
    matches (And p1 p2)  st r = matches p1 st r && matches p2 st r
    matches (Or  p1 p2)  st r = matches p1 st r || matches p2 st r
    matches (Relation v) st r = matches v st r
    matches _            _  _ = False


data Relation =
      RelEq Value Value
    | RelNe Value Value
    | RelLt Value Value
    | RelLe Value Value
    | RelGt Value Value
    | RelGe Value Value
    deriving (Eq, Show)

instance Comparator Relation where
    matches (RelEq a b) st r = comp (==) st r a b
    matches (RelNe a b) st r = comp (/=) st r a b
    matches (RelLt a b) st r = comp (<)  st r a b
    matches (RelLe a b) st r = comp (<=) st r a b
    matches (RelGt a b) st r = comp (>)  st r a b
    matches (RelGe a b) st r = comp (>=) st r a b

comp :: (Primitive -> Primitive -> Bool) -> AwkState -> Record -> Value -> Value -> Bool
comp c st r v1 v2 = c a b
    where
        a = expand st r v1
        b = expand st r v2


newtype Expr = ActionExpr
    { _actions :: [Action]
    }
    deriving (Eq, Show)


data Action =
      PrintAll
    | PrintValue [Value]
    | Assign Text Value
    deriving (Eq, Show)

instance Executor Action where
    execute PrintAll st r =
        (st, _line r <> "\n")
    execute (PrintValue vs) st r =
        (st, T.concat $ map (T.pack . show . expand st r) vs <> ["\n"])
    execute (Assign name value) st r =
        (st { sVariables = H.insert name prim $ sVariables st }, T.empty)
        where
            prim = expand st r value


data Value =
      Primitive Primitive
    | FieldVar Int
    | Variable Text
    | NumFields
    | NumRecords
    deriving (Eq, Show)

expand :: AwkState -> Record -> Value -> Primitive
expand _ _  (Primitive p)   = p
expand _ r  NumFields       = Number $ length $ _fields r
expand st _ NumRecords      = Number $ sRecords st

expand st _ (Variable name) =
    H.findWithDefault (String T.empty) name $ sVariables st

expand _ r  (FieldVar 0)    = String $ _line r
expand _ r  (FieldVar n)
        | n <= length (_fields r) = fromRight (String value) primitive
        | otherwise               = String T.empty
    where
        value = _fields r !! (n - 1)
        primitive = parse (pPrimitive <* eof) "fieldVar" value


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


-- | Parsing

pProgram :: Parser Program
pProgram = choice [try full, try exec, try grep, pEmptyProgram]
    where
        grep = Grep <$> pPattern
        full = do
            p <- pPattern
            spaces
            Full p . _actions <$> pExpr
        exec = Exec . _actions <$> pExpr

pEmptyProgram :: Parser Program
pEmptyProgram = choice [try emptyBrace, emptyString]
    where
        emptyString = spaces >> pure NoProgram
        emptyBrace = do
            spaces >> char '{' >> spaces >> char '}' >> spaces
            pure NoProgram

pPattern :: Parser Pattern
pPattern = choice [try pNot, try pCond, try regex, try rel, try begin, end]
    where
        begin = string "BEGIN" >> pure Begin
        end   = string "END"   >> pure End
        pCond = try pAnd <|> pOr
        pNorm = choice [try pNot, try pCond, try rel, regex]

        pAnd = do
            p1 <- choice [try pNot, try rel, regex]
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

pRelation :: Parser Relation
pRelation = choice [try eq, try ne, try lt, try le, try gt, ge]
    where
        ne = go "!=" RelNe
        eq = go "==" RelEq
        lt = go "<"  RelLt
        le = go "<=" RelLe
        gt = go ">"  RelGt
        ge = go ">=" RelGe

        go s b = do
            v1 <- pValue
            spaces >> string s >> spaces
            b v1 <$> pValue

pExpr :: Parser Expr
pExpr = do
    spaces >> char '{' >> spaces
    as <- sepEndBy1 pAction spaces
    spaces >> char '}' >> spaces
    pure $ ActionExpr as

pAction :: Parser Action
pAction = do
        o <- choice [try pVar, try pAll, pAssign]
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

pAssign :: Parser Action
pAssign = do
    name <- many1 alphaNum
    spaces
    _ <- char '='
    spaces
    Assign (T.pack name) <$> pValue

pAny :: Parser Primitive
pAny = String . T.pack <$> many1 anyChar

pPrimitive :: Parser Primitive
pPrimitive = choice [try quote, num]
    where
        quote = do
            _ <- char '"'
            s <- many1 (noneOf "\"")
            _ <- char '"'
            pure $ String $ T.pack s
        num = Number . read <$> many1 digit

pKeywords :: Parser String
pKeywords = choice [string "print"]

pValue :: Parser Value
pValue = choice [try sep, try field, try nf, try nr, try pr, var]
    where
        nf = NumFields <$ string "NF"
        nr = NumRecords <$ string "NR"
        pr = Primitive <$> pPrimitive
        var = do
            skipMany pKeywords
            Variable . T.pack <$> many1 alphaNum

        field = FieldVar <$> do
            _ <- char '$'
            read <$> many1 digit
        sep = do
            _ <- char ','
            pure $ Primitive $ String " "

-- | Options

type Separator = Text

data Options = Options {
      optProgram   :: Maybe Text
    , optSeparator :: Separator
    }
    deriving (Eq, Show)


defaultOptions :: Options
defaultOptions = Options {
      optProgram = Nothing
    , optSeparator = " "
    }

optionDesc :: [OptDescr (Options -> Either String Options)]
optionDesc =
    [ Option "f " ["file"]
        (ReqArg
            (\arg opt -> Right opt { optProgram = Just $ T.pack arg })
            "FILE")
        "read the awk program source from FILE instead of the command line"

    , Option "F" ["field-separator"]
        (ReqArg
            (\arg opt -> Right opt { optSeparator = T.pack arg })
            "FS")
        "use FS for the input field separator"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "awk" optionDesc))
        "show this help text"
    ]

awkMain :: [String] -> IO ()
awkMain args = do
        unless (null errors) $
            die $ unlines errors
        either die (`runAwk` arguments) $
            foldM (flip id) defaultOptions opts
    where
        arguments = map T.pack other
        (opts, other, errors) = getOpt RequireOrder optionDesc args

data RecordSource = StdinRecord | FileRecord Text
    deriving (Show, Eq)

getRecords :: RecordSource -> IO [Record]
getRecords StdinRecord    = extractRecords <$> L.getContents
getRecords (FileRecord f) = extractRecords <$> L.readFile (T.unpack f)

extractRecords :: L.Text -> [Record]
extractRecords = map (getRecord . L.toStrict) . L.lines

data Executable = Executable Separator [RecordSource] Program
    deriving (Show, Eq)

normalize :: Options -> [Text] -> Either String Executable
normalize (Options Nothing _) []        = Left "no program provided"
normalize (Options Nothing s) [p]       = builder p s [StdinRecord]
normalize (Options Nothing s) (p:files) = builder p s $ map FileRecord files
normalize (Options (Just p) s) []       = builder p s [StdinRecord]
normalize (Options (Just p) s) files    = builder p s $ map FileRecord files

builder :: Text -> Separator -> [RecordSource] -> Either String Executable
builder progSrc s sources =
    either
        (Left . show)
        (Right . Executable s sources)
        $ parse (pProgram <* eof) "awk" progSrc

runAwk :: Options -> [Text] -> IO ()
runAwk opts args = do
        options <- getOptions
        either die (void . ioAwk) $ normalize options args
    where
        getOptions = case opts of
            (Options Nothing s) -> pure $ Options Nothing s
            -- Replace the filepath with the file contents, yikes
            (Options (Just f) s) -> do
                p <- T.readFile $ T.unpack f
                pure $ Options (Just p) s

ioAwk :: Executable -> IO AwkState
ioAwk (Executable s rs p) =
        concat <$> mapM getRecords rs >>= foldM (ioExecute p) state
    where
        state = emptyState { sSeparator = s }

ioExecute :: Program -> AwkState -> Record -> IO AwkState
ioExecute p st r = do
        T.putStr newLine
        pure newState
    where
        (newState, newLine) = execute p incState r
        incState = st { sRecords = sRecords st + 1 }
