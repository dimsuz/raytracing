{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Main where

import System.IO (hPutStr, stderr)
import Data.List.Index (imapM_)
import Control.Monad (when)

imageWidth = 200
imageHeight = 100

output :: [(Int, Int, Int)] -> Int -> Int -> IO ()
output image w h = do
  putStrLn $ "P3\n" <> show w <> " " <> show h <> "\n255"
  imapM_ (\idx (ir, ig, ib) -> do
             printScanLineDebug w h idx
             putStrLn $ show ir <> " " <> show ig <> " " <> show ib
         ) image

printScanLineDebug :: Int -> Int -> Int -> IO ()
printScanLineDebug w h index = when (index `mod` w == 0) $ do
  hPutStr stderr $ "Scanlines remaining " <> show (h - (index `div` w)) <> "\r"

main :: IO ()
main = do
  let pxs = [(i, j) | j <- [imageHeight-1, imageHeight-2 .. 0], i <- [0..imageWidth-1]]
      rgbs :: [(Double, Double, Double)]
      rgbs = map (\(i,j) -> (fromIntegral i / fromIntegral imageWidth, fromIntegral j / fromIntegral imageHeight, 0.2)) pxs
      irgbs = map (\(r,g,b) -> (truncate (255.999 * r), truncate (255.999 * g), truncate (255.999 * b))) rgbs
  output irgbs imageWidth imageHeight
