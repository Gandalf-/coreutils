module AddrInfoSpec where

import           Coreutils.AddrInfo
import           Network.Socket
import           Test.Hspec

spec :: Spec
spec = do
    describe "parses" $ do
        it "socketType" $ do
            parseSocketType Nothing            `shouldBe` Right (addrSocketType defaultHints)
            parseSocketType (Just "stream")    `shouldBe` Right Stream
            parseSocketType (Just "dgram")     `shouldBe` Right Datagram
            parseSocketType (Just "raw")       `shouldBe` Right Raw
            parseSocketType (Just "rdm")       `shouldBe` Right RDM
            parseSocketType (Just "seqpacket") `shouldBe` Right SeqPacket
            parseSocketType (Just "invalid")   `shouldBe` Left "Invalid socket type"

        it "family" $ do
            parseFamily Nothing          `shouldBe` Right (addrFamily defaultHints)
            parseFamily (Just "inet")    `shouldBe` Right AF_INET
            parseFamily (Just "inet6")   `shouldBe` Right AF_INET6
            parseFamily (Just "unix")    `shouldBe` Right AF_UNIX
            parseFamily (Just "netbios") `shouldBe` Right AF_NETBIOS
            parseFamily (Just "invalid") `shouldBe` Left "Invalid address family"

        it "flags" $ do
            parseFlags os `shouldBe` []
            parseFlags os { optCanoncial = True }      `shouldBe` [AI_CANONNAME]
            parseFlags os { optNumericHost = True }    `shouldBe` [AI_NUMERICHOST]
            parseFlags os { optNumericService = True } `shouldBe` [AI_NUMERICSERV]
            parseFlags os { optPassive = True }        `shouldBe` [AI_PASSIVE]

            parseFlags os { optCanoncial = True, optNumericHost = True }
                `shouldBe` [AI_CANONNAME, AI_NUMERICHOST]
            parseFlags os { optNumericService = True, optPassive = True }
                `shouldBe` [AI_NUMERICSERV, AI_PASSIVE]

        it "protocol" $ do
            parseProtocol Nothing      `shouldBe` Right defaultProtocol
            parseProtocol (Just "17")  `shouldBe` Right 17
            parseProtocol (Just "udp") `shouldBe` Right 17
            parseProtocol (Just "tcp") `shouldBe` Right 6
            parseProtocol (Just "invalid") `shouldBe` Left "Invalid protocol"

        it "address" $ do
            cleanAddress "[2601:600:c400:30c0:d250:99ff:fec3:2321]:443"
                `shouldBe` "2601:600:c400:30c0:d250:99ff:fec3:2321 443"

            cleanAddress "[::1]:0"           `shouldBe` "::1 0"
            cleanAddress "127.0.0.1:0"       `shouldBe` "127.0.0.1 0"
            cleanAddress "192.168.122.62:80" `shouldBe` "192.168.122.62 80"

    describe "display" $
        {-
        $ getaddrinfo www.NetBSD.org
        dgram inet6 udp 2001:4f8:3:7:2e0:81ff:fe52:9ab6 0
        stream inet tcp 149.20.53.67 0
        -}
        it "works" $ do
            let body = "stream inet udp 127.0.0.1 0"
            display info      `shouldBe` body
            display canonInfo `shouldBe` "example.com\n" <> body

    where
        os = defaultOptions

        info = AddrInfo [] AF_INET Stream 17 addr Nothing
        canonInfo = info { addrCanonName = Just "example.com" }

        addr = SockAddrInet 0 host
        host = tupleToHostAddress (127, 0, 0, 1)
