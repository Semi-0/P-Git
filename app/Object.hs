module Object where 
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)
import Crypto.Hash (hashlazy, Digest, SHA1, digestFromByteString)
import qualified Data.ByteString.Lazy.Char8 as C8
import System.Directory (doesFileExist, doesDirectoryExist)
import Control.Exception (IOException, try)
import Crypto.Hash (hashlazy, Digest, SHA1, digestFromByteString)
import qualified Crypto.Hash as H
import qualified Data.ByteArray  as BA
import Control.Monad (void, filterM, unless)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Char8 (pack, unpack, split)
-- create commit
-- commit 156tree 070c6d5d918e5c051b5b8e72779b08c4eedd6da6
-- parent 4c4a4d4f2a9d3e9c1228a0940632663307c029ab
-- author John Doe <[email protected]> 1416224921 +0100
-- committer John Doe <[email protected]> 1416224921 +0100

-- This is my commit message
digestToBL :: H.Digest a -> C8.ByteString
digestToBL = BL.fromStrict . BA.convert


data CommitPerson = CommitPerson {
    personName :: BL.ByteString,
    personEmail :: BL.ByteString,
    personTimestamp ::  BL.ByteString
}

data Commit = Commit {
    treeSHA :: BL.ByteString,
    parentSHA ::  BL.ByteString,
    commitAuthor :: CommitPerson,
    commitCommitter :: CommitPerson,
    commitMessage :: BL.ByteString
}


createFileEntry :: FilePath -> Digest SHA1 -> IO TreeEntry
createFileEntry filePath sha = do
    let fileName = last $ split '/' (pack filePath)
    return $ TreeEntry  fileEntryModeValue (BL.fromStrict fileName) (digestToBL sha)

createDirectoryEntry :: FilePath -> Digest SHA1 -> IO TreeEntry
createDirectoryEntry filePath sha = do
    let dirName = last $ split '/' (pack filePath)
    return $ TreeEntry directoryEntityModeValue  (BL.fromStrict dirName) (digestToBL sha)





data TreeEntry = TreeEntry {mode :: BL.ByteString, name :: BL.ByteString, sha ::  BL.ByteString} deriving (Show, Eq)
data TreeObject = TreeObject [TreeEntry]

displayEntity :: Bool -> (TreeEntry -> Bool) -> TreeObject -> IO()
displayEntity nameOnly pred (TreeObject entries) =
        mapM_ (printEntry nameOnly) (filter pred entries)

printEntry :: Bool -> TreeEntry -> IO()
printEntry nameOnly entry =
    if nameOnly then putStrLn $ C8.unpack (name entry)
                else print entry

fileEntryModeValue :: ByteString
fileEntryModeValue = C8.pack $ "100644"

directoryEntityModeValue :: ByteString
directoryEntityModeValue = C8.pack $ "40000"

toByteStringRaw :: TreeObject -> ByteString
toByteStringRaw (TreeObject entries) = BL.concat $ map toByteStringRawEntry entries

toByteStringRawEntry :: TreeEntry -> ByteString
toByteStringRawEntry (TreeEntry mode name sha) = BL.concat [mode, C8.pack " ", name, C8.pack "\0", sha]

calculateContentSize :: TreeObject -> Int
calculateContentSize treeObject = fromIntegral $ BL.length $ toByteStringRaw treeObject

calculateContentSizeEntry :: TreeEntry -> Int
calculateContentSizeEntry entry = fromIntegral $ BL.length $ toByteStringRawEntry $ entry

addHeaderForTreeObject :: TreeObject -> ByteString
addHeaderForTreeObject treeObject = BL.append header (toByteStringRaw treeObject)
    where header = C8.pack $ "tree " ++ show (calculateContentSize treeObject) ++ "\0"

getTreeSha :: TreeObject -> Digest SHA1
getTreeSha treeObject = hashlazy $ addHeaderForTreeObject treeObject

entityIsFile :: TreeEntry -> Bool
entityIsFile = (== fileEntryModeValue) . mode

entityIsDirectory :: TreeEntry -> Bool
entityIsDirectory = (== directoryEntityModeValue) . mode


isDir :: FilePath -> IO Bool
isDir = doesDirectoryExist

isFile :: FilePath -> IO Bool
isFile = doesFileExist
