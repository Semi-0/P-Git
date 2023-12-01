module Helper where 
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Word (Word8)
import Data.Char (ord)
-- helper

charToWord8 :: Char -> Word8
charToWord8 c = fromIntegral (ord c)

c2w :: Char -> Word8
c2w = fromIntegral . ord