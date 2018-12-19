{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Array.IArray as Array
import Data.Char (digitToInt)
import Data.Either (isLeft)
import Data.Geometry.YX (YX(..))
import qualified Data.Geometry.YX as YX
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec $ do
  describe "box" $ do
    it "should support empty input" $ do
      YX.box [] `shouldBe` Nothing
    it "should support simple input" $ do
      YX.box [YX 2 1, YX 2 2, YX 1 0] `shouldBe` Just (YX 1 0, YX 2 2)
  describe "bytestring conversions" $ do
    it "should parse a simple square case" $ do
      let
        got = YX.byteStringToArray (Just . digitToInt) "12\n34\n"
        want = Array.listArray (0, 1) [1..4] :: Array.Array YX Int
      got `shouldBe` Right want
    it "fails on uneven row" $ do
      let got = YX.byteStringToArray Just "12\n3\n" :: Either String (Array.Array YX Char)
      isLeft got `shouldBe` True
    it "round trips" $ do
      let
        bs = "12\n34\n56"
        Right arr = YX.byteStringToArray Just bs :: Either String (Array.Array YX Char)
        bs' = YX.arrayToByteString id arr
      bs' `shouldBe` bs
