module CryptoLib (eea, eea', eulerPhi, eulerPhi', modInv, fermatPT, fermatPT', hashCP, modInv', modN) where

import Test.QuickCheck
import Prelude hiding (gcd)
import Data.List
import Data.Bits

-- | Returns a triple (gcd, s, t) such that gcd is the greatest common divisor
-- of a and b, and gcd = a*s + b*t.
eea :: (Int, Int) -> (Int, Int, Int)
eea = eea'
eea' (a, b)
  | (b == 0) || (a == b) = (a,1,0)
  | otherwise = do
      let (g,s,t) = eea'(b,a `mod` b)
      let a' = a `div` b
      let t' = -(a')*t + s
      (g,t,t')

gcd :: Int -> Int -> Int
gcd = gcd'
gcd' a b = let (d, _, _) = eea' (a, b) in d

-- | Returns Euler's Totient for the value n.
eulerPhi :: Int -> Int
eulerPhi =  eulerPhi'
eulerPhi' :: Integral a => a -> a
eulerPhi' n | n < 1 = 0
           | otherwise = eulerPhiAux 1 2 n

eulerPhiAux acc a n
  | a < n = let acc' = if gcd' a n  == 1 then acc + 1 else acc
            in eulerPhiAux acc' (a+1) n
  | otherwise = acc
-- | Returns the value v such that n*v = 1 (mod m).
-- Returns 0 if the modular inverse does not exist.
modInv :: (Int, Int) -> Int
modInv = fromIntegral . modInv'

modInv' :: Integral a => (a, a) -> a
modInv' (n, m)
        | n < 0 = modInv' ((toPosMod m n), m)
        | otherwise = if abs(gcd' m n) /= 1 then 0 else toPosMod m $ get3rd $ eea'(m,n)

toPosMod :: Integral a => a -> a -> a
toPosMod m n | n < 0 = toPosMod m (n+m)
             | otherwise = n

-- | Returns the third element
get3rd (_,_,i) = i

-- | Returns 0 if n is a Fermat Prime, otherwise it returns the lowest
-- Fermat Witness. Tests values from 2 (inclusive) to n/3 (exclusive).
-- Instead of picking random values a to test the primality of a number n,
-- make a start from 2 and increment it by 1 at each new iteration, until you have tested all the values below n/3
fermatPT :: Int -> Int
fermatPT = fermatPT'
fermatPT' n = case partition (\a -> gcd' n a == 1) [2..(n `div`3) - 1] of
                (coprimes, divs) ->  case allFermats coprimes of
                                        0 -> head $ divs ++ [0]
                                        witness -> minimum (witness:divs)
             where
               allFermats [] = 0
               allFermats (x:xs) | fermatTheorem n x /= 1    = x
                                 | otherwise                 = allFermats xs
               fermatTheorem n a = modN a (n-1) n

powersOfTwo = iterate (*2) 1

powersOfAN :: Integral a => a -> a -> [a]
powersOfAN a n = scanl (\acc _ -> (acc*acc) `mod` n) a [1..]

-- | a^b mod n
modN :: (Bits a, Integral a) => a -> a -> a -> a
modN a b n | b < 0 = error $ "No negative power."
               | b == 0 = 1 `mod` n
               | otherwise =
  let biggestPow = length $ takeWhile (<=b) $ fromInteger <$> powersOfTwo
      pows = reverse $ take biggestPow $ powersOfAN a n
  in modN' pows b n 1

modN' [] _ n acc = acc `mod` n
modN' pows b n acc =
  let currentBit = length pows - 1
  in if testBit b currentBit
        then modN' (tail pows) b n ((acc * head pows) `mod` n)
        else modN' (tail pows) b n acc


-- | Returns the probability that calling a perfect hash function with
-- n_samples (uniformly distributed) will give one collision (i.e. that
-- two samples result in the same hash) -- where size is the number of
-- different output values the hash function can produce.
hashCP :: (Double, Double) -> Double
hashCP (n_samples, size) = hashCP' 0 n_samples size
hashCP' numInserted 1 totalSize = numInserted / totalSize
hashCP' numInserted leftToInsert size =
  let sizeLeft = size - numInserted
      probNoInsertCollision = sizeLeft / size
      probNoCollisionLater = 1 - hashCP' (numInserted + 1) (leftToInsert - 1) size
  in (1 - (probNoInsertCollision * probNoCollisionLater))

-- QuickCheck tests.
prop_eea' :: Integer -> Integer -> Bool
prop_eea' a b = let (g, m, n) = eea' (a,b)
                in  a * m + b * n == g

prop_modN a b n = b >= 0  && n>0 ==> modN a b n == (mod (a^b) n)
