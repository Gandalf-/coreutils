
module TruncateSpec where

import           Coreutils.Truncate
import           Data.Either
import           System.IO
import           Test.Hspec

spec :: Spec
spec = do
    describe "execute" $ do
        it "absolute" $ do
            ioTest 0 (Absolute 1024) `shouldReturn` 1024
            ioTest 0 (Absolute 512)  `shouldReturn` 512

        it "reference" $ do
            ioTest 0 (Reference "LICENSE") `shouldReturn` 1063

        it "adjustment" $ do
            ioTest 512  (Adjustment   512)  `shouldReturn` 1024
            ioTest 1024 (Adjustment (-512)) `shouldReturn` 512
            ioTest 1    (Adjustment (-999)) `shouldReturn` 0

        it "rounds" $ do
            ioTest 1024 (RoundUp 1024) `shouldReturn` 1024
            ioTest 512  (RoundUp 1024) `shouldReturn` 1024
            ioTest 1024 (RoundUp 1000) `shouldReturn` 2000
            ioTest 1024 (RoundUp 512)  `shouldReturn` 1024

            {-
            BSD truncate says 3000, which seems wrong
            https://github.com/freebsd/freebsd-src/blob/main/usr.bin/truncate/truncate.c
            -}
            ioTest 4096 (RoundDown 1000) `shouldReturn` 4000
            ioTest 4096 (RoundDown 4000) `shouldReturn` 4000
            ioTest 4096 (RoundDown 4096) `shouldReturn` 4096

            ioTest 1024 (RoundUp   0) `shouldReturn` 0
            ioTest 1024 (RoundDown 0) `shouldReturn` 0

    describe "parsing" $ do
        it "errors" $ do
            parseSize "abc" `shouldSatisfy` isLeft
            parseSize ""    `shouldSatisfy` isLeft
            parseSize "++1" `shouldSatisfy` isLeft
            parseSize "+-1" `shouldSatisfy` isLeft
            parseSize "1f"  `shouldSatisfy` isLeft
            parseSize "1gg" `shouldSatisfy` isLeft

        it "absolute" $ do
            parseSize "234" `shouldBe` Right (Absolute 234)
            parseSize "0"   `shouldBe` Right (Absolute 0)

        it "adjustment" $ do
            parseSize "+10" `shouldBe` Right (Adjustment 10)
            parseSize "-10" `shouldBe` Right (Adjustment (-10))

            parseSize "+10g" `shouldBe` Right (Adjustment $ 10 * g)
            parseSize "-10m" `shouldBe` Right (Adjustment $ (-10) * m)

        it "rounds" $ do
            parseSize "%512" `shouldBe` Right (RoundUp 512)
            parseSize "/512" `shouldBe` Right (RoundDown 512)

        it "suffixes" $ do
            parseSize "1k" `shouldBe` Right (Absolute k)
            parseSize "1K" `shouldBe` Right (Absolute k)

            parseSize "1m" `shouldBe` Right (Absolute m)
            parseSize "1M" `shouldBe` Right (Absolute m)

            parseSize "1g" `shouldBe` Right (Absolute g)
            parseSize "1G" `shouldBe` Right (Absolute g)

            parseSize "1t" `shouldBe` Right (Absolute t)
            parseSize "1T" `shouldBe` Right (Absolute t)
    where
        k = 1 * 1024
        m = k * 1024
        g = m * 1024
        t = g * 1024

ioTest :: Integer -> Size -> IO Integer
ioTest before size = do
        withFile path WriteMode (`hSetFileSize` before)
        executes (Options True size) [path]
        withFile path ReadMode hFileSize
    where
        path = ".stack-work/truncate.test"
