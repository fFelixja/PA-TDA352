module CryptoLib (eea, eulerPhi, modInv, fermatPT, hashCP) where

import Data.List

-- | Returns a triple (gcd, s, t) such that gcd is the greatest common divisor
-- of a and b, and gcd = a*s + b*t.
eea :: (Int, Int) -> (Int, Int, Int)
eea (a, b)
  | (b == 0) || (a == b) = (a,1,0)
  | otherwise = do
      let (g,s,t) = eea(b,a `mod` b)
      let a' = a `div` b
      let t' = -(a')*t + s
      (g,t,t')

gcd' :: Int -> Int -> Int
gcd'  a b = if a `mod` b == 0 then b else gcd' b (a `mod` b)

-- | Returns Euler's Totient for the value n.
eulerPhi :: Int -> Int
eulerPhi n = -1

-- | Returns the value v such that n*v = 1 (mod m).
-- Returns 0 if the modular inverse does not exist.
modInv :: (Int, Int) -> Int
modInv (n, m)
        | n < 0 = modInv ((toPosMod m n), m)
        | otherwise = if abs(gcd' m n) /= 1 then 0 else toPosMod m $ get3rd $ eea(m,n)

toPosMod :: Int -> Int -> Int
toPosMod m n | n < 0 = toPosMod m (n+m)
             | otherwise = n

-- | Returns the third element
get3rd :: (Int,Int,Int) -> Int
get3rd (_,_,i) = i

-- | Returns 0 if n is a Fermat Prime, otherwise it returns the lowest
-- Fermat Witness. Tests values from 2 (inclusive) to n/3 (exclusive).
-- Instead of picking random values a to test the primality of a number n,
-- make a start from 2 and increment it by 1 at each new iteration, until you have tested all the values below n/3
fermatPT :: Int -> (Int,[Int])
fermatPT n = case partition (\a -> gcd' n a == 1) [2..(n `div`3) - 1] of
                (coprimes, divs) ->  case allFermats coprimes of
                                        0 -> (head $ divs ++ [0],coprimes)
                                        witness -> (witness,coprimes)
             where
               allFermats [] = 0
               allFermats (x:xs) | fermatTheorem n x /= 1    = x
                                 | otherwise                 = allFermats xs
               fermatTheorem n a = modN a (n-1) n

-- | a^b mod n
modN :: Int -> Int -> Int -> Int
modN a b n = modN' 1 a b n
modN' acc a 0 n = acc
modN' acc a b n = modN' ((acc*a) `mod` n) a (b-1) n

-- | Returns the probability that calling a perfect hash function with
-- n_samples (uniformly distributed) will give one collision (i.e. that
-- two samples result in the same hash) -- where size is the number of
-- different output values the hash function can produce.
hashCP :: (Double, Double) -> Double
hashCP (n_samples, size) = undefined
