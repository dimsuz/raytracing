{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Loops (iterateWhile)
import Control.Monad.State
import Data.List.Index (imapM_)
import Linear.Metric (dot, norm, normalize, quadrance)
import Linear.V3
import System.IO (hPutStr, stderr)
import System.Random

imageWidth = 400

imageHeight = 200

samplesPerPixel = 100

maxDepth = 100

maxNonInfiniteFloat :: RealFloat a => a -> a
maxNonInfiniteFloat a = encodeFloat m n
  where
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
  { cameraLowerLeftCorner :: V3 Double,
    cameraHorizontal :: V3 Double,
    cameraVertical :: V3 Double,
    cameraOrigin :: V3 Double
  }

data Ray = Ray
  { rayOrigin :: V3 Double,
    rayDirection :: V3 Double
  }
  deriving (Show)

data HitRecord = HitRecord
  { hitP :: V3 Double,
    hitT :: Double,
    hitNormal :: V3 Double,
    hitNormalIsFrontFace :: Bool,
    hitMaterial :: Material
  }

data MaterialRecord = MaterialRecord
  { matAttenuation :: V3 Double,
    matScattered :: Ray
  }

class Hittable a where
  hit :: a -> Ray -> Double -> Double -> Maybe HitRecord

type Material = Ray -> HitRecord -> State StdGen (Maybe MaterialRecord)

data Sphere = Sphere
  { sphereCenter :: V3 Double,
    sphereRadius :: Double,
    sphereMaterial :: Material
  }

instance Hittable Sphere where
  hit Sphere {..} r@Ray {..} tMin tMax =
    let oc = rayOrigin - sphereCenter
        a = norm rayDirection ^ 2
        half_b = dot oc rayDirection
        c = norm oc ^ 2 - sphereRadius * sphereRadius
        discriminant = half_b ^ 2 - a * c
        sq = sqrt discriminant
        root1 = ((- half_b) - sq) / a
        root2 = ((- half_b) + sq) / a
        testRoot root =
          if root > tMin && root < tMax
            then
              let hitP = rayAt r root
                  hitT = root
                  outwardNormal = (hitP - sphereCenter) / pure sphereRadius
                  hitNormalIsFrontFace = dot rayDirection outwardNormal < 0
                  hitNormal = if hitNormalIsFrontFace then outwardNormal else (- outwardNormal)
                  hitMaterial = sphereMaterial
               in Just HitRecord {..}
            else Nothing
     in if discriminant > 0 then testRoot root1 <|> testRoot root2 else Nothing

instance Hittable a => Hittable [a] where
  hit hs r tMin tMax = snd $ foldl (\(closestSoFar, lasth) h -> hitTest h lasth closestSoFar) (tMax, Nothing) hs
    where
      hitTest h lasth closestSoFar =
        let maybeRecord = hit h r tMin closestSoFar <|> lasth
         in (maybe closestSoFar hitT maybeRecord, maybeRecord)

materialLambertian :: V3 Double -> Material
materialLambertian albedo ray HitRecord {..} = do
  ruv <- randomUnitVector
  let scatterDirection = hitNormal + ruv
      matScattered = Ray {rayOrigin = hitP, rayDirection = scatterDirection}
      matAttenuation = albedo
  return $ Just MaterialRecord {..}

materialMetal :: V3 Double -> Double -> Material
materialMetal albedo fuzz Ray {..} HitRecord {..} = do
  v <- randomUnitVector
  let reflected = reflect (normalize rayDirection) hitNormal
      direction = reflected + pure fuzz' * v
      fuzz' = if fuzz < 1 then fuzz else 1
      matScattered = Ray {rayOrigin = hitP, rayDirection = direction}
      matAttenuation = albedo
  return $ if dot direction hitNormal > 0 then Just MaterialRecord {..} else Nothing

materialDielectric :: Double -> Material
materialDielectric refIdx Ray {..} HitRecord {..} = do
  randomDouble <- random'
  let etaiOverEtat = if hitNormalIsFrontFace then 1.0 / refIdx else refIdx
      unitDirection = normalize rayDirection
      cosTheta = min (dot (- unitDirection) hitNormal) 1.0
      sinTheta = sqrt (1.0 - cosTheta ^ 2)
      matAttenuation = pure 1.0
      matScattered =
        if etaiOverEtat * sinTheta <= 1.0
          then
            if randomDouble < schlick cosTheta etaiOverEtat
              then Ray {rayOrigin = hitP, rayDirection = reflect unitDirection hitNormal}
              else Ray {rayOrigin = hitP, rayDirection = refract unitDirection hitNormal etaiOverEtat}
          else Ray {rayOrigin = hitP, rayDirection = reflect unitDirection hitNormal}
  return $ Just MaterialRecord {..}

refract :: V3 Double -> V3 Double -> Double -> V3 Double
refract uv n etaiOverEtat =
  let cosTheta = dot (- uv) n
      rOutParallel = pure etaiOverEtat * (uv + pure cosTheta * n)
      rOutPerp = pure (sqrt (1.0 - quadrance rOutParallel)) * (- n)
   in rOutParallel + rOutPerp

reflect :: V3 Double -> V3 Double -> V3 Double
reflect v n = v - pure (2 * dot v n) * n

schlick :: Double -> Double -> Double
schlick cosine refIdx =
  let r0 = (1 - refIdx) / (1 + refIdx)
      r0sq = r0 ^ 2
   in r0sq + (1.0 - r0sq) * ((1.0 - cosine) ^ 5)

rayAt :: Ray -> Double -> V3 Double
rayAt r t = rayOrigin r + pure t * rayDirection r

mkRay :: Camera -> Double -> Double -> Ray
mkRay Camera {..} u v =
  Ray
    { rayOrigin = cameraOrigin,
      rayDirection = cameraLowerLeftCorner + pure u * cameraHorizontal + pure v * cameraVertical - cameraOrigin
    }

randomInUnitSphere :: State StdGen (V3 Double)
randomInUnitSphere = iterateWhile (\v -> norm v ^ 2 >= 1) $ randomR' (-1, 1)

randomUnitVector :: State StdGen (V3 Double)
randomUnitVector = do
  a <- randomR' (0.0, 2.0 * pi)
  z <- randomR' (-1.0, 1.0)
  let r = sqrt (1.0 - z ^ 2)
  return $ V3 (r * cos a) (r * sin a) z

randomInHemisphere :: V3 Double -> State StdGen (V3 Double)
randomInHemisphere normal = do
  v <- randomInUnitSphere
  return $ if dot v normal > 0.0 then v else (- v)

rayColor :: Hittable h => h -> Ray -> State (StdGen, Int) (V3 Double)
rayColor h r = do
  depth <- gets snd
  if depth <= 0
    then return (V3 0.0 0.0 0.0)
    else case hit h r 0.00001 infinity of
      Just hrec -> rayHitColor h r hrec
      Nothing -> return $ backgroundColor r

rayHitColor :: Hittable h => h -> Ray -> HitRecord -> State (StdGen, Int) (V3 Double)
rayHitColor h r hrec@HitRecord {..} = do
  (g, depth) <- get
  let (matrec, newg) = runState (hitMaterial r hrec) g
  put (newg, depth - 1)
  case matrec of
    Just MaterialRecord {..} -> (matAttenuation *) <$> rayColor h matScattered
    Nothing -> pure 0.0

backgroundColor :: Ray -> V3 Double
backgroundColor r =
  let (V3 _ y _) = normalize $ rayDirection r
      t = 0.5 * (y + 1.0)
   in pure (1.0 - t) * V3 1.0 1.0 1.0 + pure t * V3 0.5 0.7 1.0

output :: [V3 Double] -> Int -> Int -> IO ()
output image w h = do
  putStrLn $ "P3\n" <> show w <> " " <> show h <> "\n255"
  let colors = map (sampledOutputColor samplesPerPixel) image
  imapM_
    ( \idx (V3 ir ig ib) -> do
        printScanLineDebug w h idx
        putStrLn $ show ir <> " " <> show ig <> " " <> show ib
    )
    colors

sampledOutputColor :: Int -> V3 Double -> V3 Int
sampledOutputColor samplesPerPixel rgb =
  let scale = 1.0 / fromIntegral samplesPerPixel
      (V3 r g b) = sqrt <$> pure scale * rgb
   in truncate <$> pure 256 * V3 (clamp r 0.0 0.999) (clamp g 0.0 0.999) (clamp b 0.0 0.999)

printScanLineDebug :: Int -> Int -> Int -> IO ()
printScanLineDebug w h index =
  when (index `mod` w == 0)
    $ hPutStr stderr
    $ "Scanlines remaining " <> show (h - (index `div` w)) <> "               \r"

random' :: Random a => State StdGen a
random' = state random

randomR' :: Random a => (a, a) -> State StdGen a
randomR' range = state (randomR range)

randoms' :: Random a => Int -> State StdGen [a]
randoms' n = replicateM n random'

-- TODO figure out a better name, it's not background ray!
backgroundRay :: Camera -> (Int, Int) -> State StdGen [Ray]
backgroundRay camera (i, j) = do
  rus <- if samplesPerPixel > 1 then randoms' samplesPerPixel else pure [0.0]
  rvs <- if samplesPerPixel > 1 then randoms' samplesPerPixel else pure [0.0]
  let us = map (\ru -> (ru + fromIntegral i) / fromIntegral imageWidth) rus
      vs = map (\rv -> (rv + fromIntegral j) / fromIntegral imageHeight) rvs
  return $ zipWith (mkRay camera) us vs

buildColor :: Hittable h => h -> [Ray] -> State StdGen (V3 Double)
buildColor h rs = do
  gen <- get
  let (colors, (newg, _)) = flip runState (gen, maxDepth) $ mapM (rayColor h) rs
  put newg
  return (sum colors)

camera =
  Camera
    { cameraLowerLeftCorner = V3 (-2.0) (-1.0) (-1.0),
      cameraHorizontal = V3 4.0 0.0 0.0,
      cameraVertical = V3 0.0 2.0 0.0,
      cameraOrigin = V3 0.0 0.0 0.0
    }

main :: IO ()
main = do
  gen <- getStdGen
  let pxs = [(i, j) | j <- [imageHeight -1, imageHeight -2 .. 0], i <- [0 .. imageWidth -1]]
      world =
        [ Sphere (V3 0.0 0.0 (-1.0)) 0.5 (materialLambertian (V3 0.1 0.2 0.5)),
          Sphere (V3 0.0 (-100.5) (-1.0)) 100 (materialLambertian (V3 0.8 0.8 0.0)),
          Sphere (V3 1.0 0.0 (-1.0)) 0.5 (materialMetal (V3 0.8 0.6 0.2) 0.3),
          Sphere (V3 (-1.0) 0.0 (-1.0)) 0.5 (materialDielectric 1.5),
          Sphere (V3 (-1.0) 0.0 (-1.0)) (-0.45) (materialDielectric 1.5)
        ]
      (rays, newgen) = flip runState gen $ mapM (backgroundRay camera) pxs
      colors = flip evalState newgen $ mapM (buildColor world) rays
  output colors imageWidth imageHeight
