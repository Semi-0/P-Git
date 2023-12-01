module TCPClient(
   withConnection
 , send
 , receive
) where 


import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Network.Socket hiding                    (recv, send)
import Network.Socket.ByteString                (recv, sendAll)
import Data.Monoid                              (mempty, mappend)
import Numeric                                  (readHex)

withConnection :: HostName -> ServiceName -> (Socket -> IO b) -> IO b
withConnection host port consumer = do
    sock <- openConnection host port
    r <- consumer sock
    close sock
    return r


send :: Socket -> String -> IO ()
send sock msg = sendAll sock $ C.pack msg


-- | Read packet lines.
receive :: Socket -> IO C.ByteString
receive sock = receive' sock mempty
    where receive' s acc = do
            maybeLine <- readPacketLine s
            maybe (return acc) (receive' s . mappend acc) maybeLine

-- =================================================================================

openConnection :: HostName -> ServiceName -> IO Socket
openConnection host port = do
        addrinfos <- getAddrInfo Nothing (Just host) (Just port)
        let serveraddr = head addrinfos
        sock <- socket (addrFamily serveraddr) Stream defaultProtocol
        connect sock (addrAddress serveraddr)
        return sock

-- | Read a git packet line (variable length binary string prefixed with the overall length). 
-- Length is 4 byte, hexadecimal, padded with 0.
readPacketLine :: Socket -> IO (Maybe C.ByteString)
readPacketLine sock = do
        len <- readFully mempty 4
        if C.null len then return Nothing else -- check for a zero length return -> server disconnected
            case readHex $ C.unpack len of
                ((l,_):_) | l > 4 -> do
                     line <- readFully mempty (l-4)
                     return $ Just line
                _                 -> return Nothing
    where readFully acc expected = do
            line <- recv sock expected
            let len  = C.length line
                acc' = acc `mappend` line
                cont = len /= expected && not (C.null line)
            if cont then readFully acc' (expected - len) else return acc'