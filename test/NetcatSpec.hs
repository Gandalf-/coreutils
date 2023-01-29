module NetcatSpec where

import           Coreutils.Netcat
import           Data.Either
import           Network.Socket
import           System.IO
import           Test.Hspec

spec :: Spec
spec = do
    basics

    describe "port parsing" $ do
        it "works" $ do
            parsePort "3489" `shouldBe` Right 3489
            parsePort "1" `shouldBe` Right 1
            parsePort "65535" `shouldBe` Right 65535

        it "errors" $ do
            parsePort "junk" `shouldSatisfy` isLeft
            parsePort "0" `shouldSatisfy` isLeft
            parsePort "65536" `shouldSatisfy` isLeft
            parsePort "999999" `shouldSatisfy` isLeft

    describe "port range parsing" $ do
        it "works" $ do
            parsePortRange "4-10" `shouldBe` Right [4..10]
            parsePortRange "4-4" `shouldBe` Right [4]

        it "errors" $ do
            parsePortRange "junk" `shouldSatisfy` isLeft
            parsePortRange "-5" `shouldSatisfy` isLeft
            parsePortRange "5-" `shouldSatisfy` isLeft
            parsePortRange "5-junk" `shouldSatisfy` isLeft
            parsePortRange "junk-5" `shouldSatisfy` isLeft
            parsePortRange "10-5" `shouldSatisfy` isLeft

basics :: Spec
basics = do
    describe "inet pair" $ do
        it "v4 tcp works" $ do
            pair <- v4TcpSocketPair
            pingPong pair

            pair2 <- tcpSocketPair AF_UNSPEC "127.0.0.1"
            pingPong pair2

        xit "v4 udp works" $ do
            (l, r) <- udpSocketPair AF_INET "127.0.0.1"
            a <- socketToHandle l ReadWriteMode
            b <- socketToHandle r ReadWriteMode
            hPutStrLn a "ping"
            hGetLine b `shouldReturn` "ping"

        it "v6 tcp works" $ do
            pair1 <- v6TcpSocketPair
            pingPong pair1

            pair2 <- tcpSocketPair AF_UNSPEC "::1"
            pingPong pair2

    describe "unix pair" $ do
        it "tcp works" $ do
            pair <- socketPair AF_UNIX Stream defaultProtocol
            pingPong pair

        it "udp works" $ do
            pair <- socketPair AF_UNIX Datagram defaultProtocol
            pingPong pair


pingPong :: (Socket, Socket) -> IO ()
pingPong (l, r) = do
    a <- socketToHandle l ReadWriteMode
    b <- socketToHandle r ReadWriteMode
    hPutStrLn a "ping"
    hPutStrLn b "pong"
    hGetLine b `shouldReturn` "ping"
    hGetLine a `shouldReturn` "pong"
    hClose a
    hClose b

v4TcpSocketPair :: IO (Socket, Socket)
v4TcpSocketPair = tcpSocketPair AF_INET "127.0.0.1"

v6TcpSocketPair :: IO (Socket, Socket)
v6TcpSocketPair = tcpSocketPair AF_INET6 "::1"

udpSocketPair :: Family -> ServiceName -> IO (Socket, Socket)
udpSocketPair family name = do
    sList <- socket family Datagram defaultProtocol
    cSock <- socket family Datagram defaultProtocol

    sAddr:_ <- getAddrInfo Nothing (Just name) (Just testPort)
    setSocketOption sList ReuseAddr 1
    bind sList (addrAddress sAddr)

    connect cSock (addrAddress sAddr)

    pure (sList, cSock)

tcpSocketPair :: Family -> ServiceName -> IO (Socket, Socket)
tcpSocketPair family name = do
    let hints = defaultHints {
        addrSocketType = Stream,
        addrFamily = family,
        addrProtocol = 6
    }
    addr:_ <- getAddrInfo (Just hints) (Just name) (Just testPort)
    sList <- openSocket addr
    cSock <- openSocket addr

    setSocketOption sList ReuseAddr 1
    bind sList (addrAddress addr)
    listen sList 1

    connect cSock (addrAddress addr)
    (sSock, _) <- accept sList
    close sList

    pure (sSock, cSock)

testPort :: String
testPort = "3488"
