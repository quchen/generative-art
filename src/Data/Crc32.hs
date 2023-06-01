module Data.Crc32 (
      crc32
    , Crc32(..)
) where



import           Data.Bits
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Vector          (Vector, (!))
import qualified Data.Vector          as V
import           Data.Word
import           Text.Printf



table :: Vector Word32
table = createCrc32Table 0xedb88320

createCrc32Table :: Word32 -> Vector Word32
createCrc32Table poly = V.generate 256 $ \n -> V.foldl'
    (\acc _ -> if odd acc
        then poly `xor` shiftR acc 1
        else shiftR acc 1
        )
    (fromIntegral n)
    (V.enumFromN 0 8)

-- fn crc32_compute_table() -> [u32; 256] {
--     let mut crc32_table = [0; 256];
--
--     for n in 0..256 {
--         crc32_table[n as usize] = (0..8).fold(n as u32, |acc, _| {
--             match acc & 1 {
--                 1 => 0xedb88320 ^ (acc >> 1),
--                 _ => acc >> 1,
--             }
--         });
--     }
--
--     crc32_table
-- }

newtype Crc32 = Crc32 Word32
    deriving (Eq, Ord)

instance Show Crc32 where show (Crc32 crc) = printf "%#08x" crc

-- | CRC32 checksum.
--
-- >>> import Data.ByteString.Lazy.Char8
-- >>> crc32 (pack "The quick brown fox jumps over the lazy dog")
-- 0x414fa339
--
-- >>> crc32 (pack "123456789")
-- 0xcbf43926
crc32 :: ByteString -> Crc32
crc32 bs =
    let Crc32 crcResult = BSL.foldl' (\(Crc32 crc) byte -> Crc32 (xor (crc `shiftR` 8) (table ! fromIntegral (fromIntegral (crc .&. 0xff) `xor` byte)))) (Crc32 0xffffffff) bs
    in Crc32 (complement crcResult)
