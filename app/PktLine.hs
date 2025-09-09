{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module PktLine where 

import qualified Data.ByteString.Lazy as L
import Prelude 
import Text.Printf (printf)
import qualified Prelude as P
import qualified Data.ByteString.Lazy.Char8 as C8
import Numeric (readHex)

-- 3
-- Repository data exchange in git happens in multiple phases:

-- 3.0 Paccket Line Format
-- git’s protocol payload makes extensive use of the so called packet line (or pkt-line as used in the technical documentation) format.
-- A pkt-line is a variable length binary string with the length encoded in the first four bytes of the pkt-line.

-- Create a packet line prefixed with the overall length. Length is 4 byte, hexadecimal, padded with 0

-- Git uses the pkt-line format for a few reasons:

-- Efficiency: The pkt-line format is a binary format, which is more efficient to process than text-based formats. This is important for a version control system like Git, which needs to handle potentially large amounts of data.
-- Flexibility: The pkt-line format is variable-length, which means it can accommodate data of different sizes. This is useful for Git, which needs to handle different types of data, from small metadata to large file contents.
-- Simplicity: The pkt-line format is relatively simple, with the length of the data encoded in the first four bytes. This makes it easy to parse, which is important for a system like Git that is used by many different types of software.
-- Compatibility: The pkt-line format is compatible with both binary and text data, which is important for Git, which needs to handle both types of data.

-- Helper function to convert a number to hexadecimal

flushPkt :: C8.ByteString
flushPkt = "0000"

delimiterPkt :: C8.ByteString
delimiterPkt = "0001"

reponseEndPkt :: C8.ByteString
reponseEndPkt = "0002"

toHex :: Int -> String
toHex = printf "%x"

-- Helper function to pad a string with zeros to make it 4 characters long
padWithZeros :: String -> String
padWithZeros str = printf "%04s" str

-- Function to compute the hexadecimal length of a string, padded with zeros
hexLength :: String -> String
hexLength msg = padWithZeros $ toHex (P.length msg + 4)

-- Function to format the packet line
formatPktLine :: String -> String -> String
formatPktLine len msg = len ++ msg

-- Main function to compute the packet line
pktLine :: String -> String
pktLine msg = formatPktLine (hexLength msg) msg


data PacketLine = PacketLine {
    length    :: Int
  , content   :: String
} deriving (Eq, Show)

parsePacket :: L.ByteString -> [PacketLine]
parsePacket = map parsePacketLine . C8.lines
    where parsePacketLine line = PacketLine (readHexString $ C8.unpack $ L.take 4 line) (C8.unpack $ L.drop 4 line)
          readHexString = fst . head . readHex    


encodeBodyToPktLine :: [C8.ByteString] -> L.ByteString
encodeBodyToPktLine body = L.concat $ map encodeElement body

encodeElement :: C8.ByteString -> L.ByteString
encodeElement element 
    | element == delimiterPkt = delimiterPkt
    | element == flushPkt = flushPkt
    | otherwise = (C8.pack . pktLine . C8.unpack) element

-- this is not right seems it missed a few capabilities need to do more research about it 
getCapabilities :: Maybe C8.ByteString -> Maybe [C8.ByteString]
getCapabilities = fmap (\r -> case C8.split '\NUL' r of
    (_:caps:_) ->  C8.words $ C8.takeWhile (/= '\n') caps
    _ -> [])          