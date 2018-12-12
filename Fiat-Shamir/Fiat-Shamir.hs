{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Binary as B (encode)
import qualified Data.ByteString.Char8 as BS (unpack, concat)
import qualified Data.ByteString.Lazy as BL (toChunks)
import qualified Data.Text as T (splitOn, pack, unpack)

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
recoverMessage :: Integer -> Integer -> [[Integer]] -> Integer
recoverMessage n pubX runs =
  -- TODO. Return x such that x^2 = pubX (mod n).
  n
