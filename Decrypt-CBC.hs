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
