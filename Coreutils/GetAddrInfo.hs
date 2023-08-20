module Coreutils.GetAddrInfo where

import           Control.Monad
import           Coreutils.Util
import           Data.Char
import           Network.Socket
import           System.Console.GetOpt
import           System.Exit
import           Text.Read             (readMaybe)

data GetAddrInfo = GetAddrInfo

instance Util GetAddrInfo where
    run _ = getAddrInfoMain

-- | IO

getAddrInfoMain :: [String] -> IO ()
getAddrInfoMain args = do
        unless (null errors) $
            die $ unlines errors
        either die (`runAddrInfo` host) $
            foldM (flip id) defaultOptions opts
    where
        (opts, other, errors) = getOpt RequireOrder optionDesc args
        host
            | null other = Nothing
            | otherwise  = Just $ head other


runAddrInfo :: Options -> Maybe String -> IO ()
runAddrInfo os host = do
    hints <- either die return $ getHints os
    ais <- getAddrInfo (Just hints) host (optService os)
    mapM_ (putStrLn . display) ais

display :: AddrInfo -> String
display ai = canonical <> unwords
    [ map toLower $ show $ addrSocketType ai
    , cleanFamily $ show $ addrFamily ai
    , protocolToName $ addrProtocol ai
    , show $ addrAddress ai
    ]
    where
        canonical = maybe "" (++ "\n") (addrCanonName ai)

        cleanFamily :: String -> String
        cleanFamily ('A':'F':'_':xs) = map toLower xs
        cleanFamily xs               = xs

protocolToName :: ProtocolNumber -> String
protocolToName 6  = "tcp"
protocolToName 17 = "udp"
protocolToName p  = show p

-- | Parsing

getHints :: Options -> Either String AddrInfo
getHints os = do
    socketType <- parseSocketType $ optSockType os
    family <- parseFamily $ optFamily os
    protocol <- parseProtocol $ optProtocol os
    pure defaultHints
        { addrSocketType = socketType
        , addrFamily = family
        , addrFlags = parseFlags os
        , addrProtocol = protocol
        }

parseSocketType :: Maybe String -> Either String SocketType
parseSocketType t = case t of
    Nothing          -> pure $ addrSocketType defaultHints
    Just "stream"    -> pure Stream
    Just "dgram"     -> pure Datagram
    Just "raw"       -> pure Raw
    Just "rdm"       -> pure RDM
    Just "seqpacket" -> pure SeqPacket
    Just _           -> Left "Invalid socket type"

parseFamily :: Maybe String -> Either String Family
parseFamily f = case f of
    Nothing -> pure $ addrFamily defaultHints
    Just fam -> do
        case readMaybe ("AF_" ++ map toUpper fam) of
            (Just family) -> pure family
            Nothing       -> Left "Invalid address family"

parseFlags :: Options -> [AddrInfoFlag]
parseFlags os =
    foldr (\(f, m) acc -> if m then f:acc else acc) []
        [ (AI_CANONNAME,   optCanoncial      os)
        , (AI_NUMERICHOST, optNumericHost    os)
        , (AI_NUMERICSERV, optNumericService os)
        , (AI_PASSIVE,     optPassive        os)
        ]

parseProtocol :: Maybe String -> Either String ProtocolNumber
-- Consider parsing /etc/protocols on Unix
parseProtocol (Just "tcp") = pure 6
parseProtocol (Just "udp") = pure 17
parseProtocol p = case p of
    Nothing -> pure defaultProtocol
    Just p' -> case readMaybe p' of
        (Just protocol) -> pure protocol
        Nothing         -> Left "Invalid protocol"

-- | Options

data Options = Options
    { optCanoncial      :: Bool
    , optNumericService :: Bool
    , optNumericHost    :: Bool
    , optPassive        :: Bool
    , optFamily         :: Maybe String
    , optProtocol       :: Maybe String
    , optService        :: Maybe String
    , optSockType       :: Maybe String
    }

defaultOptions :: Options
defaultOptions = Options
    { optCanoncial = False
    , optFamily = Nothing
    , optNumericService = False
    , optNumericHost = False
    , optPassive = False
    , optProtocol = Nothing
    , optService = Nothing
    , optSockType = Nothing
    }

optionDesc :: [OptDescr (Options -> Either String Options)]
optionDesc =
    [ Option "c" []
        (NoArg (\o -> Right o { optCanoncial = True }))
        "Look up a caonical name for the host"

    , Option "f" []
        (ReqArg (\arg o -> Right o { optFamily = Just arg }) "family")
        "Specify an address family: inet"

    , Option "N" []
        (NoArg (\o -> Right o { optNumericService = True }))
        "Treat the service as numeric and do not attempt service name resolution"

    , Option "n" []
        (NoArg (\o -> Right o { optNumericHost = True }))
        "Treat the host as a numeric address and do not attempt name resolution"

    , Option "P" []
        (NoArg (\o -> Right o { optPassive = True }))
        "Passive mode: return an address suitable for bind(2)"

    , Option "p" []
        (ReqArg (\x o -> Right o { optProtocol = Just x }) "protocol")
        "Specify a protocol: tcp, udp"

    , Option "s" []
        (ReqArg (\x o -> Right o { optService = Just x }) "service")
        "Specify a service: http, smtp"

    , Option "t" []
        (ReqArg (\x o -> Right o { optSockType = Just x }) "socktype")
        "Specify a socket type: stream, dgram"
    ]
