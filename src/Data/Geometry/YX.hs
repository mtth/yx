{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Bitmap-friendly XY coordinates.
--
-- We use @YX@ rather than @XY@ to allow natural row major order (first row sorts before the second,
-- etc.). Note that rows are assumed to go down with @y@.
module Data.Geometry.YX (
  -- * Coordinate type
  YX(..),
  -- * Basic steps
  up, left, right, down,
  steps4, steps8,
  -- * Box
  Box, box, arrayBox, boundingBox,
  boxBounds, topLeft, bottomRight,
  boxHeight, boxWidth,
  inBox, boxRange, boxRows, boxIntersection,
  boxNeighbors4, boxNeighbors8,
  -- * Transformations
  Center(..), Direction(..), rotate,
  Axis(..), mirror,
  -- * Serialization
  byteStringToArray, byteStringToArrayM, arrayToByteString
) where

import Prelude hiding (lines)

import Algebra.Lattice (Lattice(..), joinLeq, meetLeq)
import Control.Monad.Except (MonadError, throwError)
import Data.Array.IArray (IArray)
import qualified Data.Array.IArray as IArray
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (foldl')
import Data.Ix (Ix)
import qualified Data.Ix as Ix
import Data.List (groupBy)

-- | A 2D coordinate.
--
-- YX implements 'Num'. Integers are converted to their diagonal equivalent (for example @2@ becomes
-- @YX 2 2@).
data YX = YX { y :: !Int, x :: !Int } deriving (Eq, Ord, Show)

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
    [ YX y0 x0 | y0 <- Ix.range (yl, yu), x0 <- Ix.range (xl, xu) ]
  index (YX yl xl, YX yu xu) (YX y0 x0) =
    Ix.index (yl, yu) y0 * Ix.rangeSize (xl, xu) + Ix.index (xl, xu) x0
  inRange (YX yl xl, YX yu xu) (YX y0 x0) =
    Ix.inRange (yl, yu) y0 && Ix.inRange (xl, xu) x0

instance Lattice YX where
  YX y0 x0 /\ YX y1 x1 = YX (min y0 y1) (min x0 x1)
  YX y0 x0 \/ YX y1 x1 = YX (max y0 y1) (max x0 x1)

-- | Decrement @y@.
up :: YX
up = YX (-1) 0

-- | Decrement @x@.
left :: YX
left = YX 0 (-1)

-- | Increment @x@.
right :: YX
right = YX 0 1

-- | Increment @y@.
down :: YX
down = YX 1 0

-- | Ordered array of the 4 base steps.
steps4 :: [YX]
steps4 = [up, left, right, down]

-- | Ordered array of the 8 steps (4 base and 4 diagonal).
steps8 :: [YX]
steps8 = [up + left, up, up + right, left, right, down + left, down, down + right]

-- | A non-empty 2D box.
--
-- A box might have zero width or height but will always contain at least one point.
data Box = Box { _topLeft :: !YX , _bottomRight :: !YX } deriving (Eq, Show)

-- | @since 0.0.4.1
instance Semigroup Box where
  (Box tl1 br1) <> (Box tl2 br2) = Box (tl1 /\ tl2) (br1 \/ br2)

-- | Constructs a box from its extremities, returning 'Nothing' if the points are not ordered
-- appropriately.
box
  :: YX -- ^ Top-left point.
  -> YX -- ^ Bottom-right point.
  -> Maybe Box
box yx0 yx1 =
  if joinLeq yx0 yx1 && meetLeq yx0 yx1
    then Just $ Box yx0 yx1
    else Nothing

-- | Returns the box corresponding to an array, or 'Nothing' if the array is empty.
arrayBox :: IArray a e => a YX e -> Maybe Box
arrayBox = uncurry box . IArray.bounds

-- | Returns the smallest 'Box' containing all input coordinates.
boundingBox :: Foldable f => f YX -> Maybe Box
boundingBox = foldl' go Nothing where
  go Nothing yx = Just $ Box yx yx
  go (Just (Box tl br)) yx = Just $ Box (tl /\ yx) (br \/ yx)

-- | Returns the top-left most point of the box (i.e. its lattice meet).
topLeft :: Box -> YX
topLeft = _topLeft

-- | Returns the bottom-right most point of the box (i.e. its lattice join).
bottomRight :: Box -> YX
bottomRight = _bottomRight

-- | Returns the box' bounds, @(topLeft, bottomRight)@.
--
-- @since 0.0.4.1
boxBounds :: Box -> (YX, YX)
boxBounds (Box tl br) = (tl, br)

-- | Returns the height of the box, always non-negative.
boxHeight :: Box -> Int
boxHeight (Box (YX y0 _) (YX y1 _)) = y1 - y0

-- | Returns the width of the box, always non-negative.
boxWidth :: Box -> Int
boxWidth (Box (YX _ x0) (YX _ x1)) = x1 - x0

-- | Returns all coordinates within the box, sorted.
boxRange :: Box -> [YX]
boxRange (Box tl br) = Ix.range (tl, br)

-- | Returns whether a given point is within a box.
inBox :: YX -> Box -> Bool
inBox yx (Box tl br) = joinLeq yx br && meetLeq tl yx

-- | Returns the box' coordinates, sorted and grouped by row.
boxRows :: Box -> [[YX]]
boxRows (Box tl br) = groupBy (\(YX y1 _) (YX y2 _) -> y1 == y2) $ Ix.range (tl, br)

boxNeighbors :: [YX] -> Box -> YX -> [YX]
boxNeighbors steps b yx = filter (`inBox` b) $ fmap (+ yx) steps

-- | Returns 4 neighbors of YX filtered to members of the box.
--
-- @since 0.0.4.0
boxNeighbors4 :: Box -> YX -> [YX]
boxNeighbors4 = boxNeighbors steps4

-- | Returns 8 neighbors of YX filtered to members of the box.
--
-- @since 0.0.4.0
boxNeighbors8 :: Box -> YX -> [YX]
boxNeighbors8 = boxNeighbors steps8

-- | Intersects two boxes.
boxIntersection :: Box -> Box -> Maybe Box
boxIntersection (Box tl0 br0) (Box tl1 br1) =
  if joinLeq tl0 tl1 && meetLeq br0 br1
    then Just $ Box (tl0 /\ tl1) (br0 \/ br1)
    else Nothing

-- | The center of a rotation.
--
-- Valid rotations can have either an exact coordinate as center or the top left corner of a
-- coordinate.
data Center = Around YX | AroundTopLeftCorner YX deriving (Eq, Ord, Show)

-- | A rotational direction.
data Direction = Clockwise | CounterClockwise deriving (Bounded, Eq, Enum, Ord, Show)

-- | Rotates a coordinate.
rotate :: Direction -> Center -> YX -> YX
rotate dir (Around yx0) yx1 =
  let YX y2 x2 = yx1 - yx0
  in case dir of
    Clockwise -> yx0 + YX x2 (-y2)
    CounterClockwise -> yx0 + YX (-x2) y2
rotate dir (AroundTopLeftCorner yx0) yx1 = rotate dir (Around yx0) yx1 + left

-- | Symmetry axis.
data Axis
  = AboveRow Int
  | AtRow Int
  | LeftOfColumn Int
  | AtColumn Int
  deriving (Eq, Ord, Show)

-- | Flips coordinates symmetrically on the given axis.
mirror :: Axis -> YX -> YX
mirror (AboveRow y0) yx = mirror (AtRow y0) yx + up
mirror (AtRow y0) (YX y1 x1) = YX (2 * y0 - y1) x1
mirror (LeftOfColumn x0) yx = mirror (AtColumn x0) yx + left
mirror (AtColumn x0) (YX y1 x1) = YX y1 (2 * x0 - x1)

-- | Parses a newline delimited bytestring into an array in an effectful way.
--
-- @since 0.0.4.0
byteStringToArrayM
  :: (IArray a e, MonadError String m)
  => (YX -> Char -> m e) -> ByteString -> m (a YX e)
byteStringToArrayM f bs = shape (BS.split '\n' bs) (-1) >>= materialize bs where
  shape [] (YX y0 x0) = pure (YX y0 (max x0 0))
  shape rows@(row : rows') yx@(YX y0 x0)
    | null $ filter (not . BS.null) rows = shape [] yx -- Empty trailing rows.
    | otherwise = let x1 = BS.length row - 1
                  in if x1 /= x0 && x0 >= 0
                    then throwError $ "bad row lengths: " ++ show x0 ++ ", " ++ show x1
                    else shape rows' (YX (y0 + 1) x1)
  materialize bs' yx = let t = (0, yx) in IArray.listArray t <$> elems (Ix.range t) bs'
  elems yxs bs' = sequenceA $ fmap (uncurry f) $ zip yxs (filter (/= '\n') $ BS.unpack bs')

-- | Parses a newline delimited bytestring into an array.
byteStringToArray :: (IArray a e) => (Char -> Maybe e) -> ByteString -> Either String (a YX e)
byteStringToArray f bs = byteStringToArrayM (const toElem) bs where
  toElem c = case f c of
    Just e -> Right e
    Nothing -> Left $ "unknown char: " ++ show c

-- | Serializes an array into a bytestring. This function is the reverse of 'byteStringToArray'.
arrayToByteString :: (IArray a e) => (e -> Char) -> a YX e -> ByteString
arrayToByteString f arr = BS.intercalate "\n" lines where
  lines = fmap (BS.pack . fmap (f . (arr IArray.!))) . boxRows . uncurry Box . IArray.bounds $ arr
