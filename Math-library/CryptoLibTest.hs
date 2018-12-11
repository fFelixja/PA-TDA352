-- Compilation: ghc -o CryptoLibTest CryptoLibTest.hs
-- Running: ./CryptoLibTest

module Main where

import CryptoLib

main = do
  test_eea
  test_eulerPhi
  test_modInv
  test_fermatPT
  test_hashCP

test_eea :: IO ()
test_eea = do
  let tests = [ ((5, 5), (5, 1, 0))
              , ((18, 1), (1, 0, 1))
              , ((1, 18), (1, 1, 0))
              , ((21, 22), (1, -1, 1))
              , ((157, 111), (1, -41, 58))
              , ((6, 68), (2, -11, 1))
              , ((12, 36), (12, 1, 0))
              , ((42, 25), (1, 3, -5))
              , ((150, 340), (10, -9, 4))
              ]
  testit tests eea "eea" (==)
  
test_eulerPhi :: IO ()
test_eulerPhi = do
  let tests = [ (-1, 0)
		          , (13, 12)
		          , (57, 36)
		          , (111, 72)
		          , (1000, 400)
		          , (157, 156)
              ]
  testit tests eulerPhi "eulerPhi" (==)

test_modInv :: IO ()
test_modInv = do
  let tests = [ ((25, 42), 37)
              , ((11, 20), 11)
              , ((13, 50), 27)
              , ((8954, 123), 59)
              , ((-9, 823), 640)
              ]
  testit tests modInv "modInv" (==)

test_fermatPT :: IO ()
test_fermatPT = do
  let tests = [ (7, 0)
		          , (12, 2)
		          , (53, 0)
		          , (111, 2)
		          , (157, 0)
		          , (158, 2)
		          , (341, 3)
		          , (1105, 5)
		          , (2821, 7)
              ]
  testit tests fermatPT "fermatPT" (==)


test_hashCP :: IO ()
test_hashCP = do
  let tests = [ ((24, 356), 0.547481)
		          , ((100, 4096), 0.704298)
		          , ((128, 128), 1)
		          , ((10, 8202), 0.00547355)
              ]
  testit tests hashCP "hashCP" (\a -> \b -> abs(a-b) < 0.00005)

  
testit :: (Show a, Show b) => [(a,b)] -> (a -> b) -> String -> (b -> b -> Bool) -> IO ()
testit tests prog name eq = do
  errors <- mapM run tests
  putStrLn $ "Total errors in " ++ name ++ ": " ++ show (foldl (+) 0 errors)
  where run (inp, exp) = do
          let out = prog inp
          if eq out exp
            then return 0
            else do putStrLn $ name ++ " failed for input " ++ show inp ++
                               ". Got " ++ show out ++ " expected was "
                               ++ show exp
                    return 1