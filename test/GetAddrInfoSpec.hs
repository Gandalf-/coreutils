module GetAddrInfoSpec where

import           Coreutils.GetAddrInfo
import           Network.Socket
import           Test.Hspec

spec :: Spec
spec = do
    describe "socketType" $
        it "works" $ do
            parseSocketType Nothing            `shouldBe` Right (addrSocketType defaultHints)
            parseSocketType (Just "stream")    `shouldBe` Right Stream
            parseSocketType (Just "dgram")     `shouldBe` Right Datagram
            parseSocketType (Just "raw")       `shouldBe` Right Raw
            parseSocketType (Just "rdm")       `shouldBe` Right RDM
            parseSocketType (Just "seqpacket") `shouldBe` Right SeqPacket
            parseSocketType (Just "invalid")   `shouldBe` Left "Invalid socket type"

    describe "parseFamily" $
        it "works" $ do
            parseFamily Nothing          `shouldBe` Right (addrFamily defaultHints)
            parseFamily (Just "inet")    `shouldBe` Right AF_INET
            parseFamily (Just "inet6")   `shouldBe` Right AF_INET6
            parseFamily (Just "unix")    `shouldBe` Right AF_UNIX
            parseFamily (Just "netbios") `shouldBe` Right AF_NETBIOS
            parseFamily (Just "invalid") `shouldBe` Left "Invalid address family"

    describe "parseFlags" $
        it "works" $ do
            parseFlags os `shouldBe` []
            parseFlags os { optCanoncial = True }      `shouldBe` [AI_CANONNAME]
            parseFlags os { optNumericHost = True }    `shouldBe` [AI_NUMERICHOST]
            parseFlags os { optNumericService = True } `shouldBe` [AI_NUMERICSERV]
            parseFlags os { optPassive = True }        `shouldBe` [AI_PASSIVE]

            parseFlags os { optCanoncial = True, optNumericHost = True }
                `shouldBe` [AI_CANONNAME, AI_NUMERICHOST]
            parseFlags os { optNumericService = True, optPassive = True }
                `shouldBe` [AI_NUMERICSERV, AI_PASSIVE]

    describe "parseProtocol" $
        it "works" $ do
            parseProtocol Nothing      `shouldBe` Right defaultProtocol
            parseProtocol (Just "17")  `shouldBe` Right 17
            parseProtocol (Just "udp") `shouldBe` Right 17
            parseProtocol (Just "tcp") `shouldBe` Right 6
            parseProtocol (Just "invalid") `shouldBe` Left "Invalid protocol"

    describe "display" $
        {-
        $ getaddrinfo www.NetBSD.org
        dgram inet6 udp 2001:4f8:3:7:2e0:81ff:fe52:9ab6 0
        stream inet tcp 149.20.53.67 0
        -}
        it "works" $ do
            let body = "stream inet udp 127.0.0.1:0"
            display info      `shouldBe` body
            display canonInfo `shouldBe` "example.com\n" <> body

    -- No support for -s service[/protocol] syntax
    where
        os = defaultOptions

        info = AddrInfo [] AF_INET Stream 17 addr Nothing
        canonInfo = info { addrCanonName = Just "example.com" }

        addr = SockAddrInet 0 host
        host = tupleToHostAddress (127, 0, 0, 1)
