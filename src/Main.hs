{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Main where

import System.IO (hPutStr, stderr)
import Data.List.Index (imapM_)
import Control.Monad (when)
import Control.Monad.Loops (iterateWhile)
import Linear.V3
import Linear.Metric (norm, normalize, dot)
import Control.Applicative ((<|>))
import Control.Monad.State
import System.Random

imageWidth = 200
imageHeight = 100
samplesPerPixel = 100
maxDepth = 50

maxNonInfiniteFloat :: RealFloat a => a -> a
maxNonInfiniteFloat a = encodeFloat m n where
    b = floatRadix a
    e = floatDigits a
    (_, e') = floatRange a
    m = b ^ e - 1
    n = e' - e

infinity = maxNonInfiniteFloat 1

clamp :: Double -> Double -> Double -> Double
clamp x min max
  | x < min = min
  | x > max = max
  | otherwise = x

data Camera = Camera
  { cameraLowerLeftCorner :: V3 Double
  , cameraHorizontal :: V3 Double
  , cameraVertical :: V3 Double
  , cameraOrigin :: V3 Double
  }

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

mkRay :: Camera -> Double -> Double -> Ray
mkRay Camera{..} u v = Ray
  { rayOrigin = cameraOrigin
  , rayDirection = cameraLowerLeftCorner + pure u * cameraHorizontal + pure v * cameraVertical - cameraOrigin
  }

randomInUnitSphere :: State StdGen (V3 Double)
randomInUnitSphere = iterateWhile (\v -> norm v ^ 2 >= 1) $ randomR' (-1.0, 1.0)

rayColor :: Hittable h => h -> Ray -> State (StdGen, Int) (V3 Double)
rayColor h r = do
  depth <- gets snd
  if depth <= 0
    then return (V3 0.0 0.0 0.0)
    else case hit h r 0.00001 infinity of
      Just hrec -> rayHitColor h hrec
      Nothing -> return $ backgroundColor r

rayHitColor :: Hittable h => h -> HitRecord -> State (StdGen, Int) (V3 Double)
rayHitColor h HitRecord{..} = do
  (g, depth) <- get
  let (ru,newg) = runState randomInUnitSphere g
  put (newg, depth - 1)
  let target = hitP + hitNormal + ru
  color <- rayColor h (Ray { rayOrigin = hitP, rayDirection = target - hitP })
  return $ pure 0.5 * color

backgroundColor :: Ray -> V3 Double
backgroundColor r = let
        (V3 _ y _) = normalize $ rayDirection r
        t = 0.5 * (y + 1.0)
        in pure (1.0 - t) * V3 1.0 1.0 1.0 + pure t * V3 0.5 0.7 1.0

output :: [V3 Double] -> Int -> Int -> IO ()
output image w h = do
  putStrLn $ "P3\n" <> show w <> " " <> show h <> "\n255"
  let colors = map (sampledOutputColor samplesPerPixel) image
  imapM_ (\idx (V3 ir ig ib) -> do
             printScanLineDebug w h idx
             putStrLn $ show ir <> " " <> show ig <> " " <> show ib
         ) colors

sampledOutputColor :: Int -> V3 Double -> V3 Int
sampledOutputColor samplesPerPixel rgb = let
  scale = 1.0 / fromIntegral samplesPerPixel
  (V3 r g b) = sqrt <$> pure scale * rgb
  in truncate <$> pure 256 * V3 (clamp r 0.0 0.999) (clamp g 0.0 0.999) (clamp b 0.0 0.999)

printScanLineDebug :: Int -> Int -> Int -> IO ()
printScanLineDebug w h index = when (index `mod` w == 0) $
  hPutStr stderr $ "Scanlines remaining " <> show (h - (index `div` w)) <> "               \r"

random' :: Random a => State StdGen a
random' = state random

randomR' :: Random a => (a, a) -> State StdGen a
randomR' range = state (randomR range)

randoms' :: Random a => Int -> State StdGen [a]
randoms' n = replicateM n random'

-- TODO figure out a better name, it's not background ray!
backgroundRay :: Camera -> (Int, Int) -> State StdGen [Ray]
backgroundRay camera (i,j) = do
  rus <- if samplesPerPixel > 1 then randoms' samplesPerPixel else pure [0.0]
  rvs <- if samplesPerPixel > 1 then randoms' samplesPerPixel else pure [0.0]
  let
    us = map (\ru -> (ru + fromIntegral i) / fromIntegral imageWidth) rus
    vs = map (\rv -> (rv + fromIntegral j) / fromIntegral imageHeight) rvs
  return $ zipWith (mkRay camera) us vs

buildColor :: Hittable h => h -> [Ray] -> State StdGen (V3 Double)
buildColor h rs = do
  gen <- get
  let (colors, (newg, _)) = flip runState (gen, maxDepth) $ mapM (rayColor h) rs
  put newg
  return (sum colors)

camera = Camera { cameraLowerLeftCorner = V3 (-2.0) (-1.0) (-1.0)
                  , cameraHorizontal = V3 4.0 0.0 0.0
                  , cameraVertical = V3 0.0 2.0 0.0
                  , cameraOrigin = V3 0.0 0.0 0.0
                  }

main :: IO ()
main = do
  gen <- getStdGen
  let pxs = [(i, j) | j <- [imageHeight-1, imageHeight-2 .. 0], i <- [0..imageWidth-1]]
      world = [Sphere (V3 0.0 0.0 (-1.0)) 0.5, Sphere (V3 0.0 (-100.5) (-1.0)) 100]
      (rays, newgen) = flip runState gen $ mapM (backgroundRay camera) pxs
      colors = flip evalState newgen $ mapM (buildColor world) rays
  output colors imageWidth imageHeight
