{-# LANGUAGE OverloadedStrings #-}
module RSA where

import Test.QuickCheck
import CryptoLib
import Math.NumberTheory.Powers.Cubes
import qualified Data.Binary as B (encode)
import qualified Data.ByteString.Char8 as BS (unpack, concat)
import qualified Data.ByteString.Lazy as BL (toChunks)
import qualified Data.Text as T (splitOn, pack, unpack)

main = do
  let file = "RSA-input.txt"
  content <- readFile file
  let [r1,r2,r3] = parseInput content
  let m = recoverMessage r1 r2 r3
  putStrLn $ "Recovered message: " ++ show m
  putStrLn $ "Decoded message: " ++ decode m

decode :: Integer -> String
decode val = reverse (BS.unpack ((BS.concat . BL.toChunks) $ B.encode val))

-- | Parses the input of a file. Returns three lists of equal length,
-- representing the modulus, public key and cipher text of each recipient
-- respectively.
parseInput :: String -> [[Integer]]
parseInput content =
  let fileLines = take 3 $ lines content
  in  map parseLine fileLines
  where parseLine line =
          let elems  = T.splitOn "," (T.pack line)
              values = map ((!! 1) . T.splitOn "=") elems
          in map (read . T.unpack) values

-- | Try to recover the message that is encrypted three times under the
-- same public key (e = 3) but different modulus (n).
recoverMessage :: [Integer] -> [Integer] -> [Integer] -> Integer
recoverMessage [n1,e1,c1] [n2,e2,c2] [n3,e3,c3] = let (c', n') = chinese (c1,n1) (c2,n2)
                                                      (c, n)   = chinese (c',n') (c3,n3)
                                                      in integerCubeRoot c

chinese :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
-- Given x ≡ a1 mod n1
-- and   x ≡ a2 mod n2
-- where gcd(n1,n2) = 1, computes a such that
--       x ≡ a  mod n1*n2
chinese (a1, n1) (a2, n2) = let (1, m1, m2) = eea' (n1, n2)
                                n = n1 * n2
                                x = a1 * m2 * n2 + a2 * m1 * n1
                            in (x `mod` n, n)

prop_chinese :: Integer -> Integer -> Integer -> Property
prop_chinese x n1'' n2'' = n1'' /= 0 && n2'' /= 0 ==>
  let (n1', n2') = (abs n1'', abs n2'')
      (g,_,_) = eea' (n1', n2')
      (n1, n2) = (n1' `div` g, n2' `div` g)
      a1 = x `mod` n1
      a2 = x `mod` n2
      n = n1*n2
  in (x `mod` n, n) == chinese (a1, n1) (a2, n2)
