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


type Record = Text
type VariableMap = H.HashMap Text Primitive

data AwkState = AwkState
    { sVariables :: VariableMap
    , sRecords   :: Int
    , sSeparator :: Text
    }
    deriving (Eq, Show)

emptyState :: AwkState
emptyState = AwkState H.empty 0 " "

emptyRecord :: Record
emptyRecord = T.empty

fields :: AwkState -> Record -> [Text]
fields st = filter (not . T.null) . T.splitOn (sSeparator st)


class Executor a where
    execute :: a -> AwkState -> Record -> (AwkState, Text)

instance Executor a => Executor [a] where
    execute []     st _ = (st, "")
    execute [a]    st r = execute a st r
    execute (a:as) st r = (nextSt, thisText <> nextText)
        where
            prevSt = st
            (thisSt, thisText) = execute a  prevSt r
            (nextSt, nextText) = execute as thisSt r


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
    matches Begin        _  _ = True
    matches End          _  _ = True
    matches (Regex p)    _  r = r =~ p
    matches (Not p)      st r = not $ matches p st r
    matches (And p1 p2)  st r = matches p1 st r && matches p2 st r
    matches (Or  p1 p2)  st r = matches p1 st r || matches p2 st r
    matches (Relation v) st r = matches v st r


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
    { eActions :: [Action]
    }
    deriving (Eq, Show)

data Action =
      PrintAll
    | PrintValue [Value]
    | Assign    Text Value
    | AssignAdd Text Value
    | AssignSub Text Value
    | AssignMul Text Value
    | AssignDiv Text Value
    deriving (Eq, Show)

instance Executor Action where
    execute PrintAll st r =
        (st, r <> "\n")
    execute (PrintValue vs) st r =
        (st, T.concat $ map (T.pack . show . expand st r) vs <> ["\n"])

    execute (Assign name value) st r =
        (st { sVariables = H.insert name prim $ sVariables st }, T.empty)
        where
            prim = expand st r value

    execute (AssignAdd name value) st r = exprAssign (+) name value st r
    execute (AssignSub name value) st r = exprAssign (-) name value st r
    execute (AssignMul name value) st r = exprAssign (*) name value st r
    execute (AssignDiv name value) st r = exprAssign div name value st r

exprAssign :: (Int -> Int -> Int) -> Text -> Value -> AwkState -> Record -> (AwkState, Text)
exprAssign f name value st r =
    (st { sVariables = H.alter (Just . alter) name $ sVariables st }, T.empty)
    where
        alter Nothing  = prim
        alter (Just v) = Number $ toInt v `f` toInt prim
        prim = expand st r value


data Value =
      Primitive Primitive
    | FieldVar Int
    | Variable Text
    | Expression Expression
    | NumFields
    | NumRecords
    deriving (Eq, Show)

expand :: AwkState -> Record -> Value -> Primitive
expand _  _ (Primitive p) = p
expand st r NumFields     = Number $ length $ fields st r
expand st _ NumRecords    = Number $ sRecords st

expand _  _ (Expression _)  = undefined
expand st _ (Variable name) =
    H.findWithDefault (String T.empty) name $ sVariables st

expand _  r (FieldVar 0)    = String r
expand st r (FieldVar n)
        | n <= length fs = fromRight (String value) primitive
        | otherwise      = String T.empty
    where
        fs = fields st r
        value = fs !! (n - 1)
        primitive = parse (pPrimitive <* eof) "fieldVar" value


data Expression =
      Add Expression Expression
    | Sub Expression Expression
    | Val Value
    deriving (Eq, Show)


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

-- Integral requires Real (requires Num, Ord), Enum
toInt :: Primitive -> Int
toInt (Number n) = n
toInt (String _) = 0


awkMain :: [String] -> IO ()
awkMain args = do
        unless (null errors) $
            die $ unlines errors
        either die (`runAwk` arguments) $
            foldM (flip id) defaultOptions opts
    where
        arguments = map T.pack other
        (opts, other, errors) = getOpt RequireOrder optionDesc args

data RecordSource =
        StdinRecord
      | FileRecord Text
      | TestRecord [Text]
    deriving (Show, Eq)

getRecords :: RecordSource -> IO [Record]
getRecords StdinRecord     = extractRecords <$> L.getContents
getRecords (FileRecord f)  = extractRecords <$> L.readFile (T.unpack f)
getRecords (TestRecord ts) = pure ts

extractRecords :: L.Text -> [Record]
extractRecords = map L.toStrict . L.lines

runAwk :: Options -> [Text] -> IO ()
runAwk opts args = do
        options <- getOptions
        either die (void . ioAwk) $ normalize options args
    where
        getOptions = case opts of
            (Options Nothing s vs) -> pure $ Options Nothing s vs
            -- Replace the filepath with the file contents, yikes
            (Options (Just f) s vs) -> do
                p <- T.readFile $ T.unpack f
                pure $ Options (Just p) s vs

ioAwk :: Executable -> IO AwkState
ioAwk (Executable state rs (FullProgram bs ms es)) = do
        bState <-
            foldM (ioExecutes id bs) state brs
        mState <-
            concat <$> mapM getRecords rs
            >>= foldM (ioExecutes incRecords ms) bState
        foldM (ioExecutes id es) mState ers
    where
        brs = replicate (length bs) emptyRecord
        ers = replicate (length es) emptyRecord

type StateUpdate = AwkState -> AwkState

incRecords :: StateUpdate
incRecords st = st { sRecords = sRecords st + 1 }

ioExecutes :: StateUpdate -> [Program] -> AwkState -> Record -> IO AwkState
ioExecutes update ps prevState r =
        foldM (\st p -> ioExecute update p st r) prevState ps

ioExecute :: StateUpdate -> Program -> AwkState -> Record -> IO AwkState
ioExecute update p st r = do
        T.putStr newLine
        pure newState
    where
        (newState, newLine) = execute p (update st) r


-- | Parsing

type ProgramBins = ([Program], [Program], [Program])

pFullProgram :: Parser FullProgram
pFullProgram = do
        ps <- sepEndBy pProgram sep
        let (bs, ms, es) = foldl sorter ([], [], []) ps
        pure $ FullProgram bs ms es
    where
        sorter :: ProgramBins -> Program -> ProgramBins
        sorter (bs, ms, es) p@(Full Begin _) = (bs <> [p], ms, es)
        sorter (bs, ms, es) p@(Full End   _) = (bs, ms, es <> [p])
        sorter (bs, ms, es) p                = (bs, ms <> [p], es)

        sep = choice [try space, spaces >> char ';'] >> spaces

pProgram :: Parser Program
pProgram = choice [try full, try exec, try grep, pEmptyProgram]
    where
        grep = Grep <$> pPattern
        full = do
            p <- pPattern
            spaces
            Full p . eActions <$> pExpr
        exec = Exec . eActions <$> pExpr

pEmptyProgram :: Parser Program
pEmptyProgram =
    NoProgram <$ (
        spaces >> optional (char '{' >> spaces >> char '}') >> spaces
    )

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
            p1 <- choice [try pNot, try rel, regex]
            spaces >> string "||" >> spaces
            Or p1 <$> pNorm
        pNot =
            char '!' >> spaces >> Not <$> pNorm

        rel = Relation <$> pRelation
        regex = Regex . T.pack <$> between
            (char '/') (char '/') (many1 (noneOf "/"))

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
pExpr = ActionExpr <$> between
    (spaces >> char '{' >> spaces)
    (spaces >> char '}')
    (sepEndBy1 pAction spaces)

pAction :: Parser Action
pAction = do
        o <- try pVar <|> try pAll <|> pAssign
        optional (char ';')
        pure o
    where
        pAll = string "print" >> pure PrintAll
        pVar = do
            string "print" >> spaces
            PrintValue <$> sepEndBy1 pValue spaces

pAssign :: Parser Action
pAssign = do
        name <- T.pack <$> many alphaNum
        spaces
        try (unary name) <|> binary name
    where
        unary :: Text -> Parser Action
        unary name =
                (string "++" >> pure (AssignAdd name one))
            <|> (string "--" >> pure (AssignSub name one))

        one = Primitive $ Number 1

        binary name = do
            op <- string "=" <|> string "+=" <|> string "-=" <|> string "*=" <|> string "/="
            spaces
            value <- pValue
            pure $ case op of
                "="  -> Assign    name value
                "+=" -> AssignAdd name value
                "-=" -> AssignSub name value
                "*=" -> AssignMul name value
                "/=" -> AssignDiv name value
                _    -> undefined

pOptionAssign :: Parser (Text, Primitive)
-- values must already be simplified primitives
pOptionAssign = do
    name <- many1 alphaNum
    spaces >> char '=' >> spaces
    prim <- pOptionPrimitive
    pure (T.pack name, prim)

pAny :: Parser Primitive
pAny = String . T.pack <$> many1 anyChar

pExpression :: Parser Expression
pExpression = do
    v1 <- Val <$> pValue
    void spaces
    op <- anyChar
    void spaces
    v2 <- pExpression
    case op of
        '+' -> pure $ Add v1 v2
        c   -> unexpected $ "Unsupported operator: " <> [c]

pPrimitive :: Parser Primitive
pPrimitive = num <|> quote
    where
        quote = String . T.pack <$> between
            (char '"') (char '"') (many1 (noneOf "\""))
        num = Number . read <$> many1 digit

pOptionPrimitive :: Parser Primitive
-- quotes are not included for strings
pOptionPrimitive = num <|> str
    where
        str = String . T.pack <$> many1 anyChar
        num = Number . read <$> many1 digit

pKeywords :: Parser String
pKeywords = string "print"

pValue :: Parser Value
pValue = choice [try sep, try field, try nf, try nr, try pr, var]
    where
        nf = NumFields <$ string "NF"
        nr = NumRecords <$ string "NR"
        pr = Primitive <$> pPrimitive
        var = do
            skipMany pKeywords
            Variable . T.pack <$> many1 alphaNum

        field = FieldVar <$> (char '$' >> read <$> many1 digit)
        sep = do
            _ <- char ','
            pure $ Primitive $ String " "


-- | Options

type Separator = Text

data Options = Options {
      optProgram   :: Maybe Text
    , optSeparator :: Separator
    , optAssigns   :: VariableMap
    }
    deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options {
      optProgram = Nothing
    , optSeparator = " "
    , optAssigns = H.empty
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

    , Option "v" ["assign"]
        (ReqArg
            assignOption
            "var=val")
        "assign the value val to var before execution of the program begins"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "awk" optionDesc))
        "show this help text"
    ]

assignOption :: String -> Options -> Either String Options
assignOption arg opt =
    case parse (pOptionAssign <* eof) "assignment" (T.pack arg) of
        (Left err)           -> Left $ show err
        (Right (name, prim)) -> Right $ opt { optAssigns = H.insert name prim $ optAssigns opt }


data Executable = Executable AwkState [RecordSource] FullProgram
    deriving (Show, Eq)

normalize :: Options -> [Text] -> Either String Executable
normalize (Options Nothing  _ _ ) []        = Left "no program provided"
normalize (Options Nothing  s vs) [p]       = builder p s vs [StdinRecord]
normalize (Options Nothing  s vs) (p:files) = builder p s vs $ map FileRecord files
normalize (Options (Just p) s vs) []        = builder p s vs [StdinRecord]
normalize (Options (Just p) s vs) files     = builder p s vs $ map FileRecord files

builder :: Text -> Separator -> VariableMap -> [RecordSource] -> Either String Executable
builder progSrc s vs sources =
    either
        (Left . show)
        (Right . Executable state sources)
        $ parse (pFullProgram <* eof) "awk" progSrc
    where
        state = AwkState
            { sVariables = vs
            , sRecords = 0
            , sSeparator = s
            }
