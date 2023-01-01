module Coreutils.Netcat where

import           Control.Monad
import           Coreutils.Util
import           Data.Char
import           Data.List
import           Network.Socket
import           System.Console.GetOpt
import           System.Exit

data Netcat = Netcat

instance Util Netcat where
    run _ = ncMain

-- | IO

ncMain :: [String] -> IO ()
ncMain args = do
        unless (null errors) $
            die $ unlines errors
        either die (`runNc` other) $
            foldM (flip id) defaultOptions opts
    where
        (opts, other, errors) = getOpt RequireOrder optionDesc args

runNc :: Options -> [String] -> IO ()
runNc = undefined

execute :: Runtime -> IO ()
execute rt@RunListen{} = do
    sList <- socket' rt

    sAddr:_ <- getAddrInfo Nothing (Just "0.0.0.0") (Just $ show $ port rt)
    setSocketOption sList ReuseAddr 1
    bind sList (addrAddress sAddr)
    listen sList 1

    pure ()

execute _ = undefined

-- | Implementation

data Runtime =
      RunListen {
          port    :: PortNumber
        , persist :: Bool
        , socket' :: IO Socket
    }
    | RunConnect {
          host    :: ServiceName
        , ports   ::  [PortNumber]
        , socket' :: IO Socket
    }

getRuntime :: Options -> [String] -> Either String Runtime
getRuntime os xs = do
        runtime <- rt
        pure $ runtime (getSocket os)
    where
        rt
            | optAction os == Listener = listenerRuntime os xs
            | otherwise = connectorRuntime os xs

listenerRuntime :: Options -> [String] -> Either String (IO Socket -> Runtime)
listenerRuntime os [pDesc] = do
    port <- parsePort pDesc
    Right $ RunListen port (optPersist os)
listenerRuntime _ _ = Left "listener usage: [options] port"

connectorRuntime :: Options -> [String] -> Either String (IO Socket -> Runtime)
connectorRuntime _ [name, pDesc] = do
    ports <- case parsePort pDesc of
        (Left _)  -> parsePortRange pDesc
        (Right p) -> Right [p]
    Right $ RunConnect name ports
connectorRuntime _ _ = Left "connector usage: [options] hostname ports"

getSocket :: Options -> IO Socket
getSocket os = socket (optFamily os) (optSocketType os) defaultProtocol

-- | Options

protocolTcp :: ProtocolNumber
protocolTcp = 6

protocolUdp :: ProtocolNumber
protocolUdp = 17

parsePort :: String -> Either String PortNumber
parsePort p = do
    when (null p) $
        Left "Port number not provided"
    unless (all isDigit p) $
        Left "Port number is not all digits"
    let num = read p :: Integer
    unless (num > 0 && num < 65536) $
        Left "Port number out of range"
    Right $ read p

parsePortRange :: String -> Either String [PortNumber]
parsePortRange ps
        | length is /= 1 = Left "No port range provided"
        | otherwise = do
            lp <- parsePort ls
            hp <- parsePort $ tail hs
            when (lp > hp) $
                Left "Port ranges cannot be reversed"
            Right [lp .. hp]
    where
        is = elemIndices '-' ps
        (ls, hs) = splitAt (head is) ps


data Action = Listener | Connector
    deriving (Eq, Show)

data Options = Options {
      optSocketType :: SocketType
    , optFamily     :: Family
    , optProtocol   :: ProtocolNumber
    , optStdin      :: Bool
    , optAction     :: Action

    , optPersist    :: Bool
    }

defaultOptions :: Options
defaultOptions = Options {
      optSocketType = Stream
    , optFamily     = AF_UNSPEC
    , optProtocol = protocolTcp
    , optStdin = True
    , optAction = Connector
    , optPersist = False
    }

optionDesc :: [OptDescr (Options -> Either String Options)]
optionDesc =
    [ Option "u" []
        (NoArg
            (\opt -> Right opt {
                optSocketType = Datagram, optProtocol = protocolUdp
            }))
        "Use UDP instead of the default TCP"

    , Option "U" []
        (NoArg
            (\opt -> Right opt { optFamily = AF_UNIX }))
        "Use Unix Domain Sockets"

    , Option "4" []
        (NoArg
            (\opt -> Right opt { optFamily = AF_INET }))
        "Force the use of IPv4 only"

    , Option "6" []
        (NoArg
            (\opt -> Right opt { optFamily = AF_INET6 }))
        "Force the use of IPv6 only"

    , Option "d" []
        (NoArg
            (\opt -> Right opt { optStdin = False }))
        "Do not attempt to read from stdin"

    , Option "l" []
        (NoArg
            (\opt -> Right opt { optAction = Listener }))
        "Listen for an incoming connection rather than connect to a remote host"

    , Option "k" []
        (NoArg
            (\opt -> Right opt { optPersist = True }))
        "Keep listening after the first connection is completed"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "head" optionDesc))
        "Show this help text"
    ]
