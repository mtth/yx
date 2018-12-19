{-# LANGUAGE OverloadedStrings #-}

-- | A bitmap-friendly XY coordinate.
--
-- YX rather than XY since layout is row major (first row sorts before the
-- second, etc.).
module Data.Geometry.YX ( YX(..)
                        , rows
                        , up, left, right, down
                        , steps4, steps8
                        , byteStringToArray, arrayToByteString ) where

import Data.Array.IArray (IArray)
import qualified Data.Array.IArray as Array
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Ix (Ix)
import qualified Data.Ix as Ix
import Data.List (groupBy)

-- | A 2D coordinate.
--
-- YX implements 'Num'. Integers are converted to their diagonal equivalent
-- (for example @2@ becomes @YX 2 2@).
data YX = YX { x :: !Int, y :: !Int } deriving (Eq, Ord, Show)

lift1 :: (Int -> Int) -> YX -> YX
lift1 f (YX y1 x1) = YX (f y1) (f x1)

lift2 :: (Int -> Int -> Int) -> YX -> YX -> YX
lift2 f (YX y1 x1) (YX y2 x2) = YX (f y1 y2) (f x1 x2)

instance Num YX where
  (+) = lift2 (+)
  (*) = lift2 (*)
  abs = lift1 abs
  signum = lift1 signum
  fromInteger i = let i' = fromInteger i in YX i' i'
  negate = lift1 negate

instance Ix YX where
  range (YX yl xl, YX yu xu) =
    [ YX y x | y <- Ix.range (yl, yu), x <- Ix.range (xl, xu) ]
  index (YX yl xl, YX yu xu) (YX y x) =
    Ix.index (yl, yu) y * Ix.rangeSize (xl, xu) + Ix.index (xl, xu) x
  inRange (YX yl xl, YX yu xu) (YX y x) =
    Ix.inRange (yl, yu) y && Ix.inRange (xl, xu) x

-- | All coordinates, grouped by row.
rows :: (YX, YX) -> [[YX]]
rows = groupBy (\(YX y1 _) (YX y2 _) -> y1 == y2) . Ix.range

-- | Basic steps.
up, left, right, down :: YX
up = YX (-1) 0
left = YX 0 (-1)
right = YX 0 1
down = YX 1 0

-- | Ordered steps arrays.
steps4, steps8 :: [YX]
steps4 = [up, left, right, down]
steps8 = [up + left, up, up + right, left, right, down + left, down, down + right]

-- Parse newline delimited bytestring into an array.
byteStringToArray :: (IArray a e) => (Char -> Maybe e) -> ByteString -> Either String (a YX e)
byteStringToArray f bs = shape (BS.split '\n' bs) (-1) >>= materialize bs where
  shape [] (YX y x) = Right (YX y (max x 0))
  shape (row : rows) yx@(YX y x0)
    | null rows && BS.null row = shape [] yx -- Empty last row.
    | otherwise = let x = BS.length row - 1
                  in if x /= x0 && x0 >= 0
                    then Left $ "bad row lengths: " ++ show x ++ ", " ++ show x0
                    else shape rows (YX (y + 1) x)
  materialize bs yx = Array.listArray (0, yx) <$> elems bs
  elems = sequenceA . fmap parse . filter (/= '\n') . BS.unpack
  parse c = case f c of
    Just e -> Right e
    Nothing -> Left $ "unknown char: " ++ show c

-- | Reverse of `byteStringToArray`
arrayToByteString :: (IArray a e) => (e -> Char) -> a YX e -> ByteString
arrayToByteString f arr = BS.intercalate "\n" lines where
  lines = fmap (BS.pack . fmap (f . (arr Array.!))) . rows . Array.bounds $ arr
