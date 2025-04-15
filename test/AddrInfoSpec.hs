module AddrInfoSpec where

import           Coreutils.AddrInfo
import           Network.Socket
import           System.IO.Silently
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
            fmtAddress "[2601:600:c400:30c0:d250:99ff:fec3:2321]:443"
                `shouldBe` "2601:600:c400:30c0:d250:99ff:fec3:2321 443"

            fmtAddress "[::1]:0"           `shouldBe` "::1 0"
            fmtAddress "127.0.0.1:0"       `shouldBe` "127.0.0.1 0"
            fmtAddress "192.168.122.62:80" `shouldBe` "192.168.122.62 80"

        it "strips extra canonical names" $ do
            let [a, b, c] = stripCanonical [sCanonInfo, sCanonInfo, sCanonInfo]
            addrCanonName a `shouldBe` Just "example.com"
            addrCanonName b `shouldBe` Nothing
            addrCanonName c `shouldBe` Nothing

    describe "display" $ do
        {-
        $ getaddrinfo www.NetBSD.org
        dgram inet6 udp 2001:4f8:3:7:2e0:81ff:fe52:9ab6 0
        stream inet tcp 149.20.53.67 0
        -}
        it "tcp works" $ do
            let body = "stream inet tcp 127.0.0.1 0"
            display sInfo      `shouldBe` body
            display sCanonInfo `shouldBe` "canonname example.com\n" <> body

        it "udp works" $ do
            let body = "dgram inet udp 127.0.0.1 0"
            display dInfo      `shouldBe` body
            display dCanonInfo `shouldBe` "canonname example.com\n" <> body

    describe "system" $ do
        it "canonical" $ do
            (stdout, _) <- capture $ addrInfoMain ["-c", "localhost"]
            clean stdout validNames `shouldBe`
                [ "canonname localhost"
                , "dgram inet6 udp ::1 0"
                , "dgram inet udp 127.0.0.1 0"
                , "stream inet tcp 127.0.0.1 0"
                , "stream inet6 tcp ::1 0"
                ]

        it "family" $ do
            (stdout, _) <- capture $ addrInfoMain ["-f", "inet", "localhost"]
            clean stdout validNames `shouldBe`
                [ "dgram inet udp 127.0.0.1 0"
                , "stream inet tcp 127.0.0.1 0"
                ]

        it "protocol" $ do
            (stdout, _) <- capture $ addrInfoMain ["-p", "udp", "localhost"]
            clean stdout validNames `shouldBe`
                [ "dgram inet udp 127.0.0.1 0"
                , "dgram inet6 udp ::1 0"
                ]

        it "socktype" $ do
            (stdout, _) <- capture $ addrInfoMain ["-t", "stream", "localhost"]
            clean stdout validNames `shouldBe`
                [ "stream inet tcp 127.0.0.1 0"
                , "stream inet6 tcp ::1 0"
                ]

    where
        os = defaultOptions

        sInfo = AddrInfo [] AF_INET Stream 6 addr Nothing
        sCanonInfo = sInfo { addrCanonName = Just "example.com" }

        dInfo = AddrInfo [] AF_INET Datagram 17 addr Nothing
        dCanonInfo = dInfo { addrCanonName = Just "example.com" }

        addr = SockAddrInet 0 host
        host = tupleToHostAddress (127, 0, 0, 1)

        validNames = ["canonname", "dgram", "stream"]


clean :: String -> [String] -> [String]
clean xs vs = filter (isValidLine . words) . lines $ xs
    where
        isValidLine []    = False
        isValidLine (w:_) = w `elem` vs
