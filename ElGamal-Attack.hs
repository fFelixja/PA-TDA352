{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Binary as B (encode)
import qualified Data.ByteString.Char8 as BS (unpack, concat)
import qualified Data.ByteString.Lazy as BL (toChunks)
import qualified Data.Text as T (splitOn, pack, unpack)
import CryptoLib (modN, modInv', eulerPhi', fermatPT')
import Data.List

main = do
  let file = "ElGamal-input.txt"
  content <- readFile file
  let (p, g, y, year, month, day, hour, minute, second, c1, c2) = parseInput content
  let m = recoverMessage p g y year month day hour minute second c1 c2
  putStrLn $ "Recovered message: " ++ show m
  putStrLn $ "Decoded message: " ++ decode m

decode :: Integer -> String
decode val = reverse (BS.unpack ((BS.concat . BL.toChunks) $ B.encode val))

-- | Parses the problem.
parseInput :: String -> (Integer, Integer, Integer, Int, Int, Int, Int, Int, Int, Integer, Integer)
parseInput content =
  let fileLines = take 6 $ lines content
      p  = readOne (fileLines !! 0)
      g  = readOne (fileLines !! 1)
      y  = readOne (fileLines !! 2)
      t = (T.splitOn "=" (T.pack (fileLines !! 3))) !! 1
      date = (T.splitOn " " t) !! 0
      time = (T.splitOn " " t) !! 1
      year   = (read . T.unpack) $ (T.splitOn "-" date) !! 0
      month  = (read . T.unpack) $ (T.splitOn "-" date) !! 1
      day    = (read . T.unpack) $ (T.splitOn "-" date) !! 2
      hour   = (read . T.unpack) $ (T.splitOn ":" time) !! 0
      minute = (read . T.unpack) $ (T.splitOn ":" time) !! 1
      second = (read . T.unpack) $ (T.splitOn ":" time) !! 2
      c1  = readOne (fileLines !! 4)
      c2  = readOne (fileLines !! 5)
  in  (p, g, y, year, month, day, hour, minute, second, c1, c2)
  where readOne line = (read . T.unpack) $ (T.splitOn "=" (T.pack line)) !! 1

findRandomNumber :: Integer -> Integer -> Integer -> Int -> Int -> Int -> Int -> Int -> Int -> Integer
findRandomNumber c1 m g year month day hour minute second =
  let ms = [1..999]
      r =  fromIntegral (year*(10^10) + month*(10^8) + day*(10^6) + hour*(10^4) + minute*(10^2) + second)
      gr = modN g r m
      cands = scanl (\(_,acc) idx -> (idx, g*acc `mod` m)) (0,gr) ms
      (Just (key, _)) = find (\(k, gk) -> gk == c1) cands
  in key + r
    --return year*(1010) + month*(108) + day*(106) + hours*(104) + minute*(102) + second + millisecs;


recoverMessage :: Integer -> Integer -> Integer -> Int -> Int -> Int -> Int ->
                  Int -> Int -> Integer -> Integer -> Integer
recoverMessage p g y year month day hour minute second c1 c2 =
    let key = findRandomNumber c1 p g year month day hour minute second
        yInv = modInv' (y, p)
        secretInv = modN yInv key p
        in (c2 * secretInv) `mod` p
