{-# LANGUAGE RecordWildCards #-}
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
-- local protocol, SSH, native git protocol, HTTP

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

-- gonna skip capacity discovery 

-- 3.1 Reference discovery:
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







-- socket 
gitProtoRequest :: String -> String -> String
gitProtoRequest host repo = pktLine $ "git-upload-pack /" ++ repo ++ "\0host="++host++"\0"

flushPkt :: String
flushPkt = "0000"

data Remote = Remote {
    getHost         :: String
  , getPort         :: Maybe Int
  , getRepository   :: String
} deriving (Eq, Show)


lsRemote' :: Remote -> IO [PacketLine]
lsRemote' Remote{..} = withSocketsDo $
    withConnection getHost (show $ fromMaybe 9418 getPort) $ \sock -> do
        let payload = gitProtoRequest getHost getRepository
        send sock payload
        response <- receive sock
        send sock flushPkt -- Tell the server to disconnect
        return $ parsePacket $ L.fromChunks [response]

extractHostAndPort :: String -> (String, Maybe Int)
extractHostAndPort url = case parseURI url of
    Just uri -> case uriAuthority uri of
        Just auth -> (uriRegName auth, readMaybe (drop 1 (uriPort auth)))
        Nothing -> error "Invalid URL: no authority"
    Nothing -> error "Invalid URL: could not parse"

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing


lsRemote :: String -> IO [PacketLine]
lsRemote url = lsRemote' $ Remote host port repo
    where (host, port) = extractHostAndPort url
          repo = drop 1 $ dropWhile (/= '/') $ drop 1 $ dropWhile (/= '/') url

-- 3.1.1 Capacities:  
-- In Git, "capabilities" refer to the features and functionalities that a Git server supports.
-- These capabilities are communicated to the client during the initial connection, and they can include a variety of features,
--  such as support for certain Git commands, support for specific data transfer protocols, or support for specific types of data compression

-- 3.2 Packfile negotiation
-- After reference and capability discovery, client and server try to determine the minimal packfile required for the client or server to be updated.
-- upload-request    =  want-list
--         		       flush-pkt
-- want-list         =  first-want
--         		       *additional-want
-- first-want        =  PKT-LINE("want" SP obj-id SP capability-list LF)
-- additional-want   =  PKT-LINE("want" SP obj-id LF)

data Ref = Ref {
    getObjId        :: C8.ByteString
  , getRefName      :: C8.ByteString
} deriving (Show, Eq)


receivePack :: Remote -> IO ([Ref], B.ByteString)
receivePack Remote{..} = withSocketsDo $
    withConnection getHost (show $ fromMaybe 9418 getPort) $ \sock -> do
        let payload = gitProtoRequest getHost getRepository
        send sock payload
        response <- receive sock
        let pack    = parsePacket $ L.fromChunks [response]
            request = createNegotiationRequest ["multi_ack_detailed",
                        "side-band-64k",
                        "agent=git/1.8.1"] pack ++ flushPkt ++ pktLine "done\n"
        send sock request
        !rawPack <- receiveWithSideband sock (printSideband . C.unpack)
        return (mapMaybe toRef pack, rawPack)
    where printSideband str = do
                        hPutStr stderr str
                        hFlush stderr

createNegotiationRequest :: [String] -> [PacketLine] -> String
createNegotiationRequest capabilities = concatMap (++ "") . nub . map (pktLine . (++ "\n")) . foldl' (\acc e -> if null acc then first acc e else additional acc e) [] . wants . filter filterPeeledTags . filter filterRefs
                    where wants              = mapMaybe toObjId
                          first acc obj      = acc ++ ["want " ++ obj ++ " " ++ unwords capabilities]
                          additional acc obj = acc ++ ["want " ++ obj]
                          filterPeeledTags   = not . isSuffixOf "^{}" . C.unpack . ref
                          filterRefs line    = let r = C.unpack $ ref line
                                                   predicates = map ($ r) [isPrefixOf "refs/tags/", isPrefixOf "refs/heads/"]
                                               in or predicates   


receiveWithSideband :: Socket -> (B.ByteString -> IO a) -> IO B.ByteString
receiveWithSideband sock f = recrec mempty
    where recrec acc = do
            !maybeLine <- readPacketLine sock
            let skip = recrec acc
            case maybeLine of
                Just "NAK\n" -> skip -- ignore here...
                Just line -> case B.uncons line of
                                Just (1, rest)  -> recrec (acc `mappend` rest)
                                Just (2, rest)  -> f ("remote: " `C.append` rest) >> skip -- FIXME - scan for linebreaks and prepend "remote: " accordingly (see sideband.c)
                                Just (_, rest)  -> fail $ C.unpack rest
                                Nothing         -> skip
                Nothing   -> return acc

-- 3.3 Packfile transfer

clone' :: GitRepository -> Remote -> IO ()
clone' repo remote@Remote{..} = do
        (refs,packFile) <- receivePack remote
        let dir = pathForPack repo
            -- E.g. in native git this is something like .git/objects/pack/tmp_pack_6bo2La
            tmpPack = dir </> "tmp_pack_incoming"
        _ <- createDirectoryIfMissing True dir
        B.writeFile tmpPack packFile
        _ <- runReaderT (createGitRepositoryFromPackfile tmpPack refs) repo
        removeFile tmpPack
        runReaderT checkoutHead repo