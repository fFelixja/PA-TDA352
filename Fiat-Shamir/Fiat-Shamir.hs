{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Binary as B (encode)
import qualified Data.ByteString.Char8 as BS (unpack, concat)
import qualified Data.ByteString.Lazy as BL (toChunks)
import qualified Data.Text as T (splitOn, pack, unpack)
import Data.List
--import CryptoLib

main = do
  let file = "input.txt"
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
-- Re-used R:   R=18034272902982718016876556922188513994594307147106790726554982849439037529974743699623328242417617097467895140069700173749632240161140103804321447963043133103352863158863665081633529778683911733551903419551584052435920990098819993048958341639161437521361085623398559400414567170433282684194946906433189092371585379455849461613115773859071714743121021337073957821362008829041897947069757565541244804839642247895704926852713711008786074778943933653875909320591186274400582643948920161455457660939444930758382509549658248004506202636724462403521869279656659493604509021235078653077875992641797731410988235916827697165025
-- Only have one re-used R
-- TODO. Return x such that x^2 = pubX (mod n).
recoverMessage :: Integer -> Integer -> [[Integer]] -> Integer
recoverMessage n pubX runs  = let info = collectInfo runs in 0


-- | Collect __all__ R's from runs
getRs :: [[Integer]] -> [Integer]
getRs = map head

-- | Collect __all the same__ R's from runs
getSameRs :: [[Integer]] -> [Integer]
getSameRs runs = let rs = getRs runs in nub $ rs \\ nub rs

-- TODO Do we need all occurance or only one? I assume all ^^
-- Collect __all__ info from the __same__ R's in runs
collectInfo :: [[Integer]] -> [[Integer]]
collectInfo runs = let sRs = getSameRs runs in collectInfo' [[]] sRs runs --TODO change init value?
collectInfo' acc _ [] = init acc -- Throw init value,
collectInfo' acc sRs (r:rs) = if (head r) `elem` sRs then collectInfo' (r:acc) sRs rs else collectInfo' acc sRs rs


-- | if the same nonce is used twice we can compute r^-1 mod n and get x.
-- | Since we know if c and s
