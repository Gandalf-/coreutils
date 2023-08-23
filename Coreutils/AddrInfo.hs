module Coreutils.AddrInfo where

import           Control.Monad
import           Coreutils.Util
import           Data.Char
import           Data.Either.Extra
import           Data.List
import           Network.Socket
import           System.Console.GetOpt
import           System.Exit
import           Text.Read             (readMaybe)

data GetAddrInfo = GetAddrInfo

instance Util GetAddrInfo where
    run _ = addrInfoMain

-- | IO

addrInfoMain :: [String] -> IO ()
addrInfoMain args = do
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
    ais <- cleanCanonical <$> getAddrInfo (Just hints) host (optService os)
    mapM_ (putStrLn . display) ais

-- | Formatting Output

display :: AddrInfo -> String
display ai = canonical <> unwords
    [ map toLower $ show $ addrSocketType ai
    , cleanFamily $ show $ addrFamily ai
    , cleanProtocol $ addrProtocol ai
    , cleanAddress $ show $ addrAddress ai
    ]
    where
        canonical = maybe "" (++ "\n") (addrCanonName ai)

cleanCanonical :: [AddrInfo] -> [AddrInfo]
cleanCanonical []     = []
cleanCanonical (a:as) = a : map (\ai -> ai { addrCanonName = Nothing }) as

cleanFamily :: String -> String
cleanFamily ('A':'F':'_':xs) = map toLower xs
cleanFamily xs               = xs

cleanAddress :: String -> String
cleanAddress addr
    | "[" `isPrefixOf` addr =
        case break (== ']') <$> stripPrefix "[" addr of
            Just (ipv6, _:_:port) -> unwords [ipv6, port]
            _                     -> error "Invalid format"
    | otherwise =
        let (ipv4, port) = break (== ':') addr in
        unwords [ipv4, tail port]

cleanProtocol :: ProtocolNumber -> String
cleanProtocol 6  = "tcp"
cleanProtocol 17 = "udp"
cleanProtocol p  = show p

-- | Parsing Options

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
parseSocketType Nothing  = pure $ addrSocketType defaultHints
parseSocketType (Just s) = case map toLower s of
    "stream"    -> pure Stream
    "dgram"     -> pure Datagram
    "raw"       -> pure Raw
    "rdm"       -> pure RDM
    "seqpacket" -> pure SeqPacket
    _           -> Left "Invalid socket type"

parseFamily :: Maybe String -> Either String Family
parseFamily Nothing  = pure $ addrFamily defaultHints
parseFamily (Just f) = maybeToEither "Invalid address family" $ readMaybe family
    where
        family = "AF_" ++ map toUpper f

parseFlags :: Options -> [AddrInfoFlag]
parseFlags os = [ flag | (flag, condition) <- flagConditions os, condition]

flagConditions :: Options -> [(AddrInfoFlag, Bool)]
flagConditions os =
    [ (AI_CANONNAME,   optCanoncial      os)
    , (AI_NUMERICHOST, optNumericHost    os)
    , (AI_NUMERICSERV, optNumericService os)
    , (AI_PASSIVE,     optPassive        os)
    ]

parseProtocol :: Maybe String -> Either String ProtocolNumber
parseProtocol Nothing      = pure $ addrProtocol defaultHints
parseProtocol (Just "tcp") = pure 6
parseProtocol (Just "udp") = pure 17
parseProtocol (Just p)     = maybeToEither "Invalid protocol" $ readMaybe p

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
