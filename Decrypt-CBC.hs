import qualified Data.ByteString as B (unpack, pack)
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import qualified Data.Hex as H (unhex)
import Data.Word (Word8)
import System.IO.Unsafe (unsafePerformIO)
import Data.Bits

main = do
  let file = "CBC-input.txt"
  content <- readFile file
  let (first_block, encrypted) = parseInput content
  let m = recoverMessage first_block encrypted
  putStrLn $ "Recovered message: " ++ show m

-- | Parses the problem.
parseInput :: String -> ([Word8], [Word8])
parseInput content =
  let fileLines = lines content
      first_block = B.unpack (BC.pack (fileLines !! 0))
      encrypted = B.unpack (unsafePerformIO (H.unhex (BC.pack (fileLines !! 1))))
  in (first_block, encrypted)

-- | Recover the encrypted message, knowing the first block of plain text. The
-- encrypted text is of the form IV | C0 | C1 |
-- Each block is 12 bytes long.
recoverMessage :: [Word8] -> [Word8] -> String
recoverMessage first_block encrypted = do

  -- TODO. Decrypt the message on the byte (Word8) representation. When you have
  -- the final message, convert it to a string a shown below.
  BC.unpack (B.pack encrypted)

-- | IV = 6725DD9E6DE08
-- | bd = 199603177792
-- | c1 = 823C1EE8E02D6
recoverK :: [Word8] -> [Word8] -> [Word8]
recoverK fb enc = let (iv,c1) = getIVC1 enc
                          k = c1 `xor` (head fb) `xor` iv in iv:k:enc

-- TODO Add blocksize var
getIVC1 :: [Word8] -> (Word8,Word8)
getIVC1 enc = let iv = take 6 enc
                  c1 = take 6 $ drop 6 enc in (iv,c1)
blockSize = 6 -- Unit: Word8.
type Key = [Word8] -- List of `blockSize` Word8s.
type Ciphertext = [Word8] -- First block is IV.
type Plaintext = [Word8]
decryptCbc :: Key -> Ciphertext -> [Word8]
decryptCbc k ciphertext = let (iv, cs) = splitAt blockSize ciphertext
                          in concat $ decryptCbc' k iv cs
decryptCbc' _ _ [] = []
decryptCbc' k prev ciphertext | length ciphertext < blockSize = let last = take blockSize $ ciphertext ++ repeat 0 -- pad at the end.
                                                        in decryptCbc' k prev last
                              | otherwise = let (c,cs) = splitAt blockSize ciphertext
                                                m = xorWords (xorWords k prev) c
                                                ms = decryptCbc' k c cs
                                            in m:ms

xorWords :: [Word8] -> [Word8] -> [Word8]
xorWords ws1 ws2 = zipWith xor ws1 ws2
