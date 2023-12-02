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
                , requestHeaders = headers

                }
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
                    $ encodeBodyToPktLine  ( [C8.pack "command=fetch"] ++ 
                                            ["object-format=sha1"] ++
                                            [delimiterPkt] ++ 
                                            wantObjs objs ++ 
                                            [C8.pack "done"] ++ 
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