{-# LANGUAGE OverloadedStrings #-}
module FiatShamir where

import qualified Data.Binary as B (encode)
import qualified Data.ByteString.Char8 as BS (unpack, concat)
import qualified Data.ByteString.Lazy as BL (toChunks)
import qualified Data.Text as T (splitOn, pack, unpack)
import Data.List
import CryptoLib

main = do
  let file = "Fiat-Shamir-input.txt"
  content <- readFile file
  let (n, pubX, runs) = parseInput content
  let m = recoverMessage n pubX runs
  putStrLn $ "Recovered message: " ++ show m
  putStrLn $ "Decoded message: " ++ decode m

decode :: Integer -> String
decode val = reverse (BS.unpack ((BS.concat . BL.toChunks) $ B.encode val))

-- | Parses the problem.
parseInput :: String -> (Integer, Integer, [[Integer]])
parseInput content =
  let fileLines = take 12 $ lines content
      n    = readOne (fileLines !! 0)
      pubX = readOne (fileLines !! 1)
      runs = map parseLine (drop 2 fileLines)
  in  (n, pubX, runs)
  where parseLine line =
          let elems  = T.splitOn "," (T.pack line)
              values = map ((!! 1) . T.splitOn "=") elems
          in map (read . T.unpack) values
        readOne line = (read . T.unpack) $ (T.splitOn "=" (T.pack line)) !! 1

-- | Recovers the secret used in this collection of Fiat-Shamir protocol runs.
-- n = the modulus, pubX = the public key, runs = a collection of runs.
-- Each run consists of the three integers [R, c, s].
recoverMessage :: Integer -> Integer -> [[Integer]] -> Integer
recoverMessage n pubX runs  =
  -- Assuming (based on the available data) that there is only on R repeated,
  -- and it is repeated exactly twice.
  let [[_, c1, s1], [_, c2, s2]] = collectInfo runs
  in recoverX (c1, s1) (c2, s2) n

type Secret = Integer
type Challenge = Integer
type Response = Integer
recoverX :: (Challenge, Response) -> (Challenge, Response) -> Integer -> Secret
-- We assume that one message has challenge 1, the other 0.
recoverX (0, r) (1,  rs) n =
  let rInv = modInv' (r,n)
  in mod (rInv * rs) n
recoverX x x' n = recoverX x' x n


-- | Collect __all__ R's from runs
getRs :: [[Integer]] -> [Integer]
getRs = map head

-- | Collect __all the same__ R's from runs
getSameRs :: [[Integer]] -> [Integer]
getSameRs runs = let rs = getRs runs in nub $ rs \\ nub rs

-- Collect __all__ info from the __same__ R's in runs
collectInfo :: [[Integer]] -> [[Integer]]
collectInfo runs = let sRs = getSameRs runs in collectInfo' [[]] sRs runs --TODO change init value?
collectInfo' acc _ [] = init acc -- Throw init value
collectInfo' acc sRs (r:rs) = if (head r) `elem` sRs then collectInfo' (r:acc) sRs rs else collectInfo' acc sRs rs
