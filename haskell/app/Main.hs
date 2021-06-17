module Main where

import Control.Monad
import Data.JSON
import Data.Time.Clock.POSIX

main :: IO ()
main = replicateM_ 10 $ do
  (_, duration) <- parse "../json/test.json" :: IO ([Person], POSIXTime)
  putStrLn $ show (realToFrac duration * 1000) ++ "ms"
