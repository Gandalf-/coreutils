module LsSpec where

import           Coreutils.Ls
import           Data.Maybe
import           Data.Time
import           Test.Hspec

spec :: Spec
spec = parallel $ do
    describe "sorting" $ do
        it "defaults" $ do
            let rt = getRuntime defaultOptions
            sorter rt [entry "b", entry "a", entry "c"]
                `shouldBe` [entry "a", entry "b", entry "c"]
        it "reverse" $ do
            let rt = getRuntime defaultOptions { optSortReverse = True }
            sorter rt [entry "b", entry "a", entry "c"]
                `shouldBe` [entry "c", entry "b", entry "a"]
        it "nothing" $ do
            let rt = getRuntime defaultOptions { optSort = False }
            sorter rt [entry "b", entry "a", entry "c"]
                `shouldBe` [entry "b", entry "a", entry "c"]
        it "size" $ do
            let rt = getRuntime defaultOptions { optSortStrategy = SizeSorting }
            sorter rt [sEntry "a" 5, sEntry "c" 0, sEntry "b" 25]
                `shouldBe` [sEntry "b" 25, sEntry "a" 5, sEntry "c" 0]
        it "mtime" $ do
            let rt = getRuntime defaultOptions { optSortStrategy = StatSorting }
            sorter rt [mEntry "a" 5, mEntry "c" 0, mEntry "b" 25]
                `shouldBe` [mEntry "b" 25, mEntry "a" 5, mEntry "c" 0]
        it "reverse mtime" $ do
            let rt = getRuntime defaultOptions {
                optSortStrategy = StatSorting, optSortReverse = True
            }
            sorter rt [mEntry "a" 5, mEntry "c" 0, mEntry "b" 25]
                `shouldBe` [mEntry "c" 0, mEntry "a" 5, mEntry "b" 25]
        it "atime" $ do
            let rt = getRuntime defaultOptions {
                optSortStrategy = StatSorting, optSortStat = LastAccess
            }
            sorter rt [aEntry "a" 0 5, aEntry "c" 3 0, aEntry "b" 2 25]
                `shouldBe` [aEntry "b" 2 25, aEntry "a" 0 5, aEntry "c" 3 0]

    describe "getEntry" $ do
        it "defaults" $ do
            let os = defaultOptions
            getEntry os "LICENSE" `shouldReturn` entry "LICENSE"
        it "size file" $ do
            let os = defaultOptions { optSortStrategy = SizeSorting }
            getEntry os "LICENSE" `shouldReturn` sEntry "LICENSE" 1063
        it "size dir" $ do
            let os = defaultOptions { optSortStrategy = SizeSorting }
            getEntry os "src" `shouldReturn` sEntry "src" 0
        it "timestamps" $ do
            let os = defaultOptions { optSortStrategy = StatSorting }
            en <- getEntry os "LICENSE"
            mtime en `shouldSatisfy` isJust
            atime en `shouldSatisfy` isJust
            size en `shouldBe` Nothing
            perms en `shouldBe` Nothing

    describe "list" $ do
        it "defaults" $ do
            let rt = getRuntime defaultOptions
            es <- list rt "."
            "src"  `elem` es `shouldBe` True
            "."    `elem` es `shouldBe` False
            ".."   `elem` es `shouldBe` False
            ".git" `elem` es `shouldBe` False
        it "all dots" $ do
            let rt = getRuntime defaultOptions { optAllDots = True }
            es <- list rt "."
            "src"  `elem` es `shouldBe` True
            "."    `elem` es `shouldBe` True
            ".."   `elem` es `shouldBe` True
            ".git" `elem` es `shouldBe` True
        it "hidden" $ do
            let rt = getRuntime defaultOptions { optHidden = True }
            es <- list rt "."
            "src"  `elem` es `shouldBe` True
            "."    `elem` es `shouldBe` False
            ".."   `elem` es `shouldBe` False
            ".git" `elem` es `shouldBe` True
    where
        entry n = Entry n Nothing Nothing Nothing Nothing
        sEntry n size = Entry n (Just size) Nothing Nothing Nothing
        mEntry n mtime = Entry n Nothing (fromDay mtime) Nothing Nothing
        aEntry n mtime atime = Entry n Nothing (fromDay mtime) (fromDay atime) Nothing

        fromDay day = Just $ UTCTime (fromGregorian 2018 day 27) (secondsToDiffTime 0)
