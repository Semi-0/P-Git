module BasicIO where
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Control.Exception as E
import Crypto.Hash (hashlazy, Digest, SHA1, digestFromByteString)
import System.Directory (createDirectoryIfMissing, listDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>))
import Codec.Compression.Zlib (decompress, compress)
import System.FilePath (takeDirectory)
-- -- Basic IO

readObject :: String -> IO (Either E.IOException BL.ByteString)
readObject blob_sha = do
    let filePath = appendPath blob_sha
    fileExists <- doesFileExist filePath
    if fileExists
        then E.try $ do
            file_content <- BL.readFile filePath
            return $ decompress file_content
        else return $ Left $ userError "File does not exist"

writeObjectFile ::  BL.ByteString -> IO (Either E.IOException (Digest SHA1))
writeObjectFile  content = do
    let sha = hashlazy content :: Digest SHA1
    sha `seq` do
        let fileDir = appendPath (show sha)
        createDirectoryIfMissing True (takeDirectory fileDir)
        E.try $ B.writeFile fileDir (BL.toStrict $ compress content) >> return sha


-- -- Helper functions

dropUnrelevant :: BL.ByteString -> BL.ByteString
dropUnrelevant = BL.drop 8

appendPath :: String -> String
appendPath sha = ".git" </> "objects" </> dir </> file
                    where (dir, file) = splitAt 2 sha