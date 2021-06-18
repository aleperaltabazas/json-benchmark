module Main where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.JSON
import Data.Time.Extra

main :: IO ()
main = do
  let file = "../json/test.json"
  bl <- BL.readFile file
  putStrLn "AESON"
  (_, duration) <- bench 10 (return $ parseAeson bl :: IO [Person])
  putStrLn $ show (realToFrac duration * 1000) ++ "ms"
  bs <- BS.readFile file
  putStrLn "PARSEC"
  (_, duration) <- bench 10 (return $ parseParsec bs)
  putStrLn $ show (realToFrac duration * 1000) ++ "ms"

average xs = (/ (fromIntegral $ length xs)) . sum $ xs

bench :: Int -> IO a -> IO ([a], POSIXTime)
bench iterations eff = do
  res <- replicateM iterations (diff eff)
  let firsts  = map fst res
  let seconds = map snd res
  return (firsts, average seconds)
