{-# LANGUAGE OverloadedStrings #-}

import Data.Geometry.YX

import qualified Data.Array.IArray as Array
import Data.Char (digitToInt)
import Data.Either (isLeft)
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec $ do
  describe "box" $ do
    it "can be built from an empty input" $ do
      boundingBox [] `shouldBe` Nothing
    it "can be built simple inputs" $ do
      boundingBox [YX 2 1, YX 2 2, YX 1 0] `shouldBe` box (YX 1 0) (YX 2 2)
    it "checks membership" $ do
      let Just b = box (YX 1 0) (YX 2 2)
      YX 1 0 `inBox` b `shouldBe` True
      YX 1 2 `inBox` b `shouldBe` True
      YX 1 3 `inBox` b `shouldBe` False
      YX 0 0 `inBox` b `shouldBe` False
    it "returns rows" $ do
      let Just b = box (YX 1 0) (YX 2 2)
      boxRows b `shouldBe` [[YX 1 0, YX 1 1, YX 1 2], [YX 2 0, YX 2 1, YX 2 2]]

  describe "transformations" $ do
    it "can rotate a point" $ do
      rotate Clockwise (Around 0) (YX 0 5) `shouldBe` YX 5 0
    it "can mirror another" $ do
      mirror (AboveRow 0) (YX 0 5) `shouldBe` YX (-1) 5

  describe "bytestring conversions" $ do
    it "should parse a simple square case" $ do
      let
        got = byteStringToArray (Just . digitToInt) "12\n34\n"
        want = Array.listArray (0, 1) [1..4] :: Array.Array YX Int
      got `shouldBe` Right want
    it "fails on uneven row" $ do
      let got = byteStringToArray Just "12\n3\n" :: Either String (Array.Array YX Char)
      isLeft got `shouldBe` True
    it "round trips" $ do
      let
        bs = "12\n34\n56"
        Right arr = byteStringToArray Just bs :: Either String (Array.Array YX Char)
        bs' = arrayToByteString id arr
      bs' `shouldBe` bs
