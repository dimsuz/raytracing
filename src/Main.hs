{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Main where

import System.IO (hPutStr, stderr)
import Data.List.Index (imapM_)
import Control.Monad (when)
import Linear.V3
import Linear.Metric (normalize)

imageWidth = 600
imageHeight = 300

data Ray = Ray
  { rayOrigin :: V3 Double
  , rayDirection :: V3 Double
  }

rayAt :: Ray -> Double -> V3 Double
rayAt r t = rayOrigin r + pure t * rayDirection r

rayColor :: Ray -> V3 Double
rayColor r = let
  (V3 _ y _) = normalize $ rayDirection r
  t = 0.5 * (y  + 1.0)
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
      colors = map rayColor rays
  output colors imageWidth imageHeight
