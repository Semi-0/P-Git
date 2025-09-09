{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
module GitClone where
import Text.Printf (printf)
import TCPClient
import Network.Socket
import Network.HTTP.Client (
    Request (..),
    RequestBody (RequestBodyLBS),
    httpLbs,
    newManager,
    parseUrlThrow,
    responseBody,
 )

import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Prelude
import qualified Prelude as P
import qualified Data.ByteString.Lazy.Char8 as C8
import Numeric (readHex)
import Crypto.Hash (hashlazy, Digest, SHA1, digestFromByteString)
import Network.URI (parseURI, uriAuthority, uriRegName, uriPort)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (renderSimpleQuery)
import PktLine
import GHC.Generics (C)
import Control.Monad (forM, forM_, unless, when)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Word (Word8)
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Control.Arrow (Arrow(first))
import DecompressParser

-- OutLine 
-- Git Protocol: 
-- used for git client to retrieve the required set of changes 
-- from a remote git server using a variety of transport mechanisms

-- Pack File:
-- format used to transfer the minimal amount of data from the server to the client
-- and for effiecnt storage of the packed objects in repository

-- Underlying Object Store:
-- git ised fpr stpre commits, trees, tags,  and the actual file content

-- Index Format:
-- used to track changes to the files in the working directory


-- 1 
-- The Clone Process:
-- Parse the clone url to extract the host, port and repository path information.
-- Connect to the git server via TCP using the native git transport protocol.
-- Negotiate the objects that need to be transferrered from the server to the client. This includes receiving the current state of the remote repository 
-- Request the required refs and receive the pack file which contains all the objects that are reachable from the requested refs from the remote server.
-- Create a valid git repository directory and file structure on disk.
-- Store the objects and refs on disk.
-- Populate the working directory with the files and directories that represent the tip of the ref the repository points to (taking into account symlink, permissions etc).
-- Create the index file (staging area) that corresponds to that tip and the files on disk.

-- 2
-- Transport Protocol:
-- local protocol, SSH, native git protocol, HTTP, git smart protocol





-- gonna skip capacity discovery 


-- 3.0 handle the Request: 
requestTo :: String -> Request -> IO (Maybe L.ByteString)
requestTo url request = do
    manager <- newManager tlsManagerSettings
    response <- httpLbs request manager
    return $ Just $ responseBody response

postRequest :: String -> String -> RequestBody -> IO Request
postRequest url path requestBody = do
    init <- parseUrlThrow $ url <> path
    let headers = [("git-protocol", "version=2"),
                   ("accept", "application/x-git-upload-pack-result"),
                   ("content-type", "application/x-git-upload-pack-request")]
        request =  init { method = "POST",
                          requestHeaders = headers,
                          requestBody = requestBody}
    return request

-- 3.1 Capacities:  
-- In Git, "capabilities" refer to the features and functionalities that a Git server supports.
-- These capabilities are communicated to the client during the initial connection, and they can include a variety of features,
--  such as support for certain Git commands, support for specific data transfer protocols, or support for specific types of data compression

-- Discover Capacity:   

discoverCapabilities :: String -> IO (Maybe C8.ByteString)
discoverCapabilities url = do
    initReq <- parseUrlThrow  $ url <> "/info/refs"
    let params = [("service", "git-upload-pack")]
        headers =
            [ ("Accept", "application/x-git-upload-pack-advertisement")
            , ("git-protocol", "version=2")]
        request =
            initReq
                { queryString = renderSimpleQuery True params
                , requestHeaders = headers}
    requestTo url request



-- 3.2 Reference discovery:
-- client detect what data server has and server provide a list of refs
-- then client can decide whether has up-to-date or what refs need to update
-- data format look like this
-- 3a3101c62ecfbde184934f590bab5d84d7ae64a0        refs/heads/maint
-- 21ccebec0dd1d7e624ea2f22af6ac93686daf34f        refs/heads/master
-- 2c8b7bf47c81acd2a76c1f9c3be2a1f102b76d31        refs/heads/next
-- d17d3d235a4cd1cb1c6840b4a5d99d651c714cc9        refs/heads/pu
-- 5f3c2eaeab02da832953128ae3af52c6ec54d9a1        refs/heads/todo
-- d5aef6e4d58cfe1549adef5b436f3ace984e8c86        refs/tags/gitgui-0.10.0
-- 3d654be48f65545c4d3e35f5d3bbed5489820930        refs/tags/gitgui-0.10.0^{}
-- 33682a5e98adfd8ba4ce0e21363c443bd273eb77        refs/tags/gitgui-0.10.1
-- 729ffa50f75a025935623bfc58d0932c65f7de2f        refs/tags/gitgui-0.10.1^{}
-- ca9b793bda20c7d011c96895e9407fac2df9648b        refs/tags/gitgui-0.10.2


-- To initiate the ref discovery, the client establishes a TCP socket connection to the server on port 9418 and issues a single command in packet line format.
-- The ABNF for the discovery request is: ABNA( Augmented Backus-Naur Form) standards documents used for describe the structure of protocol messages and data units
-- git-proto-request = request-command SP pathname NUL [ host-parameter NUL ]
-- request-command   = "git-upload-pack" / "git-receive-pack" / "git-upload-archive"   ; case sensitive
-- pathname          = *( %x01-ff ) ; exclude NUL
-- host-parameter    = "host=" hostname [ ":" port ]

-- in packet line format it looks like:
-- 0032git-upload-pack /git-bottom-up\0host=localhost\0

-- ls-remote returns:
-- de30ca2fe16516eda94d7162e28da2d4022353b9        HEAD
-- de30ca2fe16516eda94d7162e28da2d4022353b9        refs/heads/master


-- After receiving the capability advertisement, a client can then issue a
-- request to select the command it wants with any particular capabilities
-- or arguments.  There is then an optional section where the client can
-- provide any command specific parameters or queries.  Only a single
-- command can be requested at a time.

--     request = empty-request | command-request
--     empty-request = flush-pkt
--     command-request = command
-- 		      capability-list
-- 		      delim-pkt
-- 		      command-args
-- 		      flush-pkt
--     command = PKT-LINE("command=" key LF)
--     command-args = *command-specific-arg

--     command-specific-args are packet line framed arguments defined by
--     each individual command.

-- if there is error might because havn't add unborn to the parameter


lsRefs :: String -> IO (Maybe C8.ByteString)
lsRefs url = do
    let requestBody =  RequestBodyLBS $ encodeBodyToPktLine ["command=ls-refs",
                                                            "object-format=sha1",
                                                            delimiterPkt,
                                                            "ref-prefix HEAD",
                                                            "ref-prefix refs/heads/",
                                                            "ref-prefix refs/tags/",
                                                            flushPkt]
    request <- postRequest url "/git-upload-pack" requestBody
    requestTo url request

-- parse refs: 
-- the refs 


-- 

-- 3.3 Packfile negotiation
-- After reference and capability discovery, client and server try to determine the minimal packfile required for the client or server to be updated.
-- upload-request    =  want-list
--         		       flush-pkt
-- want-list         =  first-want
--         		       *additional-want
-- first-want        =  PKT-LINE("want" SP obj-id SP capability-list LF)
-- additional-want   =  PKT-LINE("want" SP obj-id LF)


-- `fetch` is the command used to fetch a packfile in v2.  It can be looked
-- at as a modified version of the v1 fetch where the ref-advertisement is
-- stripped out (since the `ls-refs` command fills that role) and the
-- message format is tweaked to eliminate redundancies and permit easy
-- addition of future extensions.

-- Additional features not supported in the base command will be advertised
-- as the value of the command in the capability advertisement in the form
-- of a space separated list of features: "<command>=<feature 1> <feature 2>"

-- A `fetch` request can take the following arguments:

--     want <oid>
-- 	Indicates to the server an object which the client wants to
-- 	retrieve.  Wants can be anything and are not limited to
-- 	advertised objects.

--     have <oid>
-- 	Indicates to the server an object which the client has locally.
-- 	This allows the server to make a packfile which only contains
-- 	the objects that the client needs. Multiple 'have' lines can be
-- 	supplied.

--     done
-- 	Indicates to the server that negotiation should terminate (or
-- 	not even begin if performing a clone) and that the server should
-- 	use the information supplied in the request to construct the
-- 	packfile.

fetch :: String -> [C8.ByteString] -> IO (Maybe C8.ByteString)
fetch url objs = do
    let requestBody = RequestBodyLBS
                    $ encodeBodyToPktLine  (["command=fetch", 
                                            "object-format=sha1"] ++
                                            ["object-format=sha1"] ++
                                            [delimiterPkt] ++
                                            wantObjs objs ++
                                            ["done"] ++
                                            [flushPkt])
    request <-  postRequest url "/git-upload-pack" requestBody
    requestTo url request
    where wantObjs :: [C8.ByteString] -> [C8.ByteString]
          wantObjs = map (\obj -> C8.pack  "want " <> obj <> C8.pack "\n")

-- -- 3.3 Packfile transfer

-- A 12 byte pack file header with:
-- a 4-byte magic byte with the value 'P' 'A' 'C' 'K' (decimal 1346454347)
-- a 4 byte pack file version
-- a 4 byte number of objects in the packfile
-- n objects with
-- a variable length object header that contains the type of object 
-- (see below) and the length of the inflated/uncompressed data that follows
-- only for deltified objects: the 20 byte base object name 
-- (for objects of type OBJ_REF_DELTA) or a relative (negative)
-- offset from the delta object’s position in the pack for objects of type 
-- OBJ_OFS_DELTA - see below).
-- zlib deflated/compressed object data
-- A 20 byte SHA1 checksum of all of the above as trailer

-- The pack file object header uses a variant of a variable length unsigned integer encoding 
-- that contains the object type in the first byte.

-- That first byte consists of:

-- The most significant bit (MSB) that determines whether we need to read more bytes 
-- to get the encoded object size (if the MSB is set we read the next byte)
-- 3 bits with the object type (from cache.h):
-- OBJ_COMMIT = 1
-- OBJ_TREE = 2
-- OBJ_BLOB = 3
-- OBJ_TAG = 4
-- OBJ_OFS_DELTA = 6
-- OBJ_REF_DELTA = 7
-- 4 bits with the size (partial size if the MSB is set and more bytes need to be read)
-- The following bytes after the first byte contain parts of the overall length in the least 
-- significant 7 bits of the octet while the MSB is again used to indicate whether more bytes need to be read.



data Packfile = Packfile {
    magicByte :: ByteString,
    version :: Int,
    numObjects :: Int,
    objects :: [Object]
} deriving (Show)

data Object = Object {
    header :: String,
    unparsedObject :: String
} deriving (Show)

data ObjectHeader = ObjectHeader {
    objType :: PackObjType,
    objSize :: Int
} deriving (Show)

getObjType :: Int -> PackObjType
getObjType int
    | int == 1 = OBJ_COMMIT
    | int == 2 = OBJ_TREE
    | int == 3 = OBJ_BLOB
    | int == 4 = OBJ_TAG
    | int == 6 = OBJ_OFS_DELTA
    | int == 7 = OBJ_REF_DELTA
    | otherwise = UNKNOWN_OBJ_TYPE

data PackObjType
    = OBJ_COMMIT
    | OBJ_TREE
    | OBJ_BLOB
    | OBJ_TAG
    | OBJ_OFS_DELTA
    | OBJ_REF_DELTA
    | UNKNOWN_OBJ_TYPE
    deriving (Show, Eq)


packFileParser :: Parser Packfile
packFileParser = do
    magicByte <-  Data.Attoparsec.ByteString.take 4
    unless (magicByte == "PACK") $ fail "Invalid magic byte"
    version <-  parseBigEndianInt
    unless (version == 2) $ fail "Invalid version"
    numObjects <- parseBigEndianInt
    objects <- count numObjects parseObject
    return $ Packfile magicByte version numObjects objects


parseObject :: Parser Object 
parseObject = do 
    header <- objHeaderParser
    obj <- case objType header of
        OBJ_COMMIT -> parseCommit header
        OBJ_TREE -> parseTree header
        OBJ_BLOB -> parseBlob header
        OBJ_TAG -> parseTag header
        OBJ_REF_DELTA -> parseRefDelta header
        OBJ_OFS_DELTA -> fail "UnSupported Object Type" <> show OBJ_OFS_DELTA
        UNKNOWN_OBJ_TYPE -> fail "Unknown object type" 




-- 3.3.1 Object header parsing
-- The first byte is used to store both the object type and the initial size of the object.
--  This is a common technique in binary protocols to make efficient use of space.

-- Here's how it works:

-- The object type is stored in the 4th to 6th bits of the first byte.
-- The getObjTypeInt function extracts these bits by shifting the byte to the right by 4 bits (shiftR firstByte 4) 
-- and then performing a bitwise AND with 0x70 (which is 01110000 in binary) to keep only the relevant bits.
-- The initial size of the object is stored in the 1st to 4th bits of the first byte. 
-- The getObjSize function extracts these bits by performing a bitwise AND with 0x0f (which is 00001111 in binary) to keep only the relevant bits.
-- If the size is larger than 15 (which can be represented by 4 bits), the most significant bit (MSB) of the first byte is set to 1, 
-- indicating that more bytes follow to encode the full size. 
-- The getFullObjectSize function is then called to read the remaining bytes and calculate the full size.

objHeaderParser :: Parser ObjectHeader
objHeaderParser = do
    firstByte <- fromIntegral <$> anyWord8
    let objType = getObjType $ getObjTypeInt firstByte
    objSize <- getObjSize firstByte
    return $ ObjectHeader objType objSize


-- what is MSB?
-- MSB stands for Most Significant Bit. 
-- In a binary number, the MSB is the bit position with the highest value. 
-- It's the bit that's leftmost when the number is written out. For an 8-bit binary number, the MSB is the 8th bit.
-- For example, let's take the 8-bit binary number 10011011. 
-- The MSB in this case is the first 1 on the left. This bit represents the value 128 in decimal.
-- The Most Significant Bit (MSB) is used as a flag to indicate whether more data follows,
-- which can be seen as a form of efficiency because it allows the protocol to use variable-length fields.
-- If the MSB is set (1), it indicates that the current byte is not the last one and more bytes follow. 
-- If the MSB is not set (0), it indicates that the current byte is the last one.
--  This allows the protocol to use just enough bytes to represent a field, which can save space when transmitting or storing data 1, 2.

-- Consider an example where you want to represent the number 300. 
-- The binary representation of 300 is 100101100. If you were to represent this number without using the MSB to indicate more data
-- you would need a fixed-length field of 9 bits. But 9 bits might be more than you need for some numbers, wasting space.

-- On the other hand, if you use the MSB to indicate more data, you can represent the number 300 in two bytes:
 -- 10000101 and 00001100. The MSB of the first byte is set to 1, indicating that more data follows. 
--The remaining 7 bits of the first byte and the 8 bits of the second byte represent the number 300. This way, 
--you can represent numbers up to 127 with one byte and larger numbers with more bytes, saving space when the numbers are small.

-- This technique is used in various binary protocols and file formats to encode numbers in a space-efficient way.
-- It's also used in your code to encode the size of an object in the getObjSize and getFullObjectSize functions.
-- The size is initially extracted from the 4 least significant bits of the first byte. If the size is larger than 15,
-- the MSB of the first byte is set, and the remaining size is encoded in the following bytes. 
-- This allows the size to be encoded in as few bytes as possible, which is more efficient in terms of space usage.


-- This function extracts the object type from the first byte.
-- For example, if the first byte is 0x70 (01110000 in binary), 
-- the object type is 0x07 (00000111 in binary).
getObjTypeInt :: Int -> Int
getObjTypeInt firstByte = (shiftR firstByte 4) .&. 0x07

-- This function extracts the size of the object from the first byte.
-- For example, if the first byte is 0x0f (00001111 in binary), 
-- the size of the object is 0x0f (00001111 in binary).
-- If the most significant bit is set, it calls getFullObjectSize to get the full size.
getObjSize :: Int -> Parser Int
getObjSize firstByte = do
    let objSize = firstByte .&. 0xf
    if isMsbSet firstByte
    then getFullObjectSize objSize
    else return objSize

-- This function is used to get the full size of the object when the size is encoded in more than one byte.
-- It reads the next byte and adds the seven least significant bits to the size.
-- For example, if the next byte is 0x7f (01111111 in binary), 
-- the size to add is 0x7f (01111111 in binary).
-- The size to add is then shifted to the left to place it in the correct position in the final size.
-- This process is repeated until a byte is read where the most significant bit is not set.
getFullObjectSize :: Int -> Parser Int 
getFullObjectSize objSize = gfob objSize 0
    where gfob :: Int -> Int -> Parser Int
          gfob objSize iteration = do
            nextByte <- fromIntegral <$> anyWord8
            let sizeToAdd = (nextByte .&. 0x7f) `shiftL` (4 + iteration * 7)
                newSize = objSize + sizeToAdd
            if isMsbSet nextByte
            then gfob newSize (iteration + 1)
            else pure newSize

-- This function checks if the most significant bit (MSB) of an 8-bit integer is set.
-- It performs a bitwise AND operation with 0x80 (which is 10000000 in binary) to isolate the MSB.
-- If the result is not 0, then the MSB is set.
-- For example, if the input is 0x80 (10000000 in binary), the function returns True.
-- If the input is 0x7F (01111111 in binary), the function returns False. (because msb is the leftmost bit which is 0)
isMsbSet :: Int {- 8 bit int -} -> Bool
isMsbSet w = w .&. 0x80 /= 0




    -- | The 'parseBigEndianInt' function reads four bytes in big-endian format
--   and combines them into a single integer.
parseBigEndianInt ::  Parser Int
parseBigEndianInt = do
    -- Read four bytes and convert each byte to an integer.
    b1 <- fromIntegral <$> anyWord8
    b2 <- fromIntegral <$> anyWord8
    b3 <- fromIntegral <$> anyWord8
    b4 <- fromIntegral <$> anyWord8
    -- Combine the four integers into a single integer. The shiftL function
    -- moves the bits of each integer to the correct position in the final integer,
    -- and the .|. operator combines these four values into a single integer.
    return $ (b1 `shiftL` 24) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 8) .|. b4

-- 3.3.2 Object parsing    
-- -- 3.3.2.1 delta encoding 
-- The deltified representations (pack file object types 6 & 7) in the packfile use delta compression to minimize the amount of data that needs to be transferred and/or stored. Deltification only happens in pack files.
-- The following (from “File System Support for Delta Compression”) gives a good general definition of delta compression:
-- Delta compression consists of representing a target version’s contents as the mutation (delta) of some existing source contents to achieve the same goal, a reduction in space or time.
--  Typically, the target and source are related file versions and have similar contents.
-- he delta compression algorithm that is used in git was originally based on xdelta and LibXDiff but was further simplified for the git use case (see the “diff’ing files” thread on the git mailinglist).
--  The following discussion is based on the delta file format used by git and the patch-delta.c and diff-delta.c files from the git source.