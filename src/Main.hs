{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Main where

import System.IO (hPutStr, stderr)
import Data.List.Index (imapM_)
import Control.Monad (when)
import Linear.V3
import Linear.Metric (norm, normalize, dot)
import Control.Applicative ((<|>))
import Data.Semigroup
import Debug.Trace

imageWidth = 1000
imageHeight = 500

maxNonInfiniteFloat :: RealFloat a => a -> a
maxNonInfiniteFloat a = encodeFloat m n where
    b = floatRadix a
    e = floatDigits a
    (_, e') = floatRange a
    m = b ^ e - 1
    n = e' - e

infinity = maxNonInfiniteFloat 1

data Ray = Ray
  { rayOrigin :: V3 Double
  , rayDirection :: V3 Double
  } deriving Show

data HitRecord = HitRecord
  { hitP :: V3 Double
  , hitT :: Double
  , hitNormal :: V3 Double
  , hitNormalIsFrontFace :: Bool
  }

class Hittable a where
  hit :: a -> Ray -> Double -> Double -> Maybe HitRecord

data Sphere = Sphere
  { sphereCenter :: V3 Double
  , sphereRadius :: Double
  }

instance Hittable Sphere where
  hit Sphere{..} r@Ray{..} tMin tMax = let
    oc = rayOrigin - sphereCenter
    a = norm rayDirection ^ 2
    half_b = dot oc rayDirection
    c = norm oc ^ 2 - sphereRadius * sphereRadius
    discriminant = half_b ^ 2 - a * c
    sq = sqrt discriminant
    root1 = ((-half_b) - sq) / a
    root2 = ((-half_b) + sq) / a
    testRoot root = if root > tMin && root < tMax
      then let hitP = rayAt r root
               hitT = root
               outwardNormal = (hitP - sphereCenter) / pure sphereRadius
               hitNormalIsFrontFace = dot rayDirection outwardNormal < 0
               hitNormal = if hitNormalIsFrontFace then outwardNormal else (-outwardNormal)
           in Just HitRecord{..}
      else Nothing
    in if discriminant > 0 then testRoot root1 <|> testRoot root2 else Nothing

instance Hittable a => Hittable [a] where
  hit hs r tMin tMax = snd $ foldl (\(closestSoFar, lasth) h -> hitTest h lasth closestSoFar) (tMax, Nothing) hs
    where hitTest h lasth closestSoFar = let
            maybeRecord = hit h r tMin closestSoFar <|> lasth
            in (maybe closestSoFar hitT maybeRecord, maybeRecord)

rayAt :: Ray -> Double -> V3 Double
rayAt r t = rayOrigin r + pure t * rayDirection r

rayColor :: Hittable h => h -> Ray -> V3 Double
rayColor h r = case hit h r 0 infinity of
  Just HitRecord{..} -> pure 0.5 * (pure 1.0 + hitNormal)
  Nothing -> let
    (V3 _ y _) = normalize $ rayDirection r
    t = 0.5 * (y + 1.0)
    in pure (1.0 - t) * V3 1.0 1.0 1.0 + pure t * V3 0.5 0.7 1.0

output :: [V3 Double] -> Int -> Int -> IO ()
output image w h = do
  putStrLn $ "P3\n" <> show w <> " " <> show h <> "\n255"
  let colors = map (\px -> truncate <$> pure 255.999 * px) image
  imapM_ (\idx (V3 ir ig ib) -> do
             printScanLineDebug w h idx
             putStrLn $ show ir <> " " <> show ig <> " " <> show ib
         ) colors

printScanLineDebug :: Int -> Int -> Int -> IO ()
printScanLineDebug w h index = when (index `mod` w == 0) $
  hPutStr stderr $ "Scanlines remaining " <> show (h - (index `div` w)) <> "\r"

backgroundRay :: (Int, Int) -> Ray
backgroundRay (i,j) = let
  lowerLeftCorner = V3 (-2.0) (-1.0) (-1.0)
  horizontal = V3 4.0 0.0 0.0
  vertical = V3 0.0 2.0 0.0
  origin = V3 0.0 0.0 0.0
  u = fromIntegral i / fromIntegral imageWidth
  v = fromIntegral j / fromIntegral imageHeight
  in Ray { rayOrigin = origin
         , rayDirection = lowerLeftCorner + pure u * horizontal + pure v * vertical
         }

main :: IO ()
main = do
  let pxs = [(i, j) | j <- [imageHeight-1, imageHeight-2 .. 0], i <- [0..imageWidth-1]]
      rays = map backgroundRay pxs
      world = Sphere (V3 0.0 0.0 (-1.0)) 0.5
      colors = map (rayColor world) rays
  output colors imageWidth imageHeight
