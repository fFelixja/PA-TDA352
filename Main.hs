module Main where

import qualified FiatShamir as FS
import qualified CBC as CBC
import qualified RSA as RSA
import qualified ElGamal as El

main :: IO ()
main = do putStrLn "\nFiat-Shamir"
          FS.main
          putStrLn "\nCBC"
          CBC.main
          putStrLn "\nRSA"
          RSA.main
          putStrLn "\nElGamal"
          El.main
