{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE LambdaCase #-}

module Main  where

import System.Directory (createDirectoryIfMissing, listDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hPutStrLn, withFile, writeFile)
import Crypto.Hash (hashlazy, Digest, SHA1, digestFromByteString)
import Control.Exception (IOException, try)
import Control.Monad (void, filterM, unless)
import System.Directory (createDirectoryIfMissing)
import System.Environment
import System.FilePath ((</>))
import System.FilePath (takeDirectory)
import Control.Monad (guard)
import qualified Data.ByteArray  as BA

import Codec.Compression.Zlib (decompress, compress)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy  as BL
import qualified Data.ByteString as B
import qualified Crypto.Hash as H
import System.Directory (doesFileExist, doesDirectoryExist)
import Data.ByteString.Char8 (pack, unpack, split)
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Word (Word8)
import Data.Char (ord)
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.RWS (MonadState(put, get))
import Data.Char (chr)
import GHC.IO.Device (RawIO(write))
import qualified Control.Exception as E
import Data.List (sort)
import Data.Maybe (catMaybes)
import Control.Concurrent.Async (mapConcurrently)
import qualified Data.Time as DF
import qualified Data.Time as DC
import qualified Data.ByteString.Builder as B

-- -- Init

initGitFile :: IO()
initGitFile = do
    let createParents = True
    gitExists <- doesDirectoryExist ".git"
    unless gitExists $ do
        createDirectoryIfMissing createParents ".git"
        createDirectoryIfMissing createParents (".git" </> "objects")
        createDirectoryIfMissing createParents (".git" </> "refs")
        headExists <- doesFileExist (".git" </> "HEAD")
        unless headExists $ withFile (".git" </> "HEAD") WriteMode $ \f -> hPutStrLn f "ref: refs/heads/master"
    putStrLn $ "Initialized git directory"


-- -- Basic IO

readObject :: String -> IO (Either IOException ByteString)
readObject blob_sha = do
    let filePath = appendPath blob_sha
    fileExists <- doesFileExist filePath
    if fileExists
        then E.try $ do
            file_content <- BL.readFile filePath
            return $ decompress file_content
        else return $ Left $ userError "File does not exist"

writeObjectFile ::  ByteString -> IO (Either IOException (Digest SHA1))
writeObjectFile  content = do
    let sha = hashlazy content :: Digest SHA1
    sha `seq` do
        let fileDir = appendPath (show sha)
        createDirectoryIfMissing True (takeDirectory fileDir)
        E.try $ B.writeFile fileDir (BL.toStrict $ compress content) >> return sha


-- -- CatFile

dropUnrelevant :: ByteString -> ByteString
dropUnrelevant = BL.drop 8

appendPath :: String -> String
appendPath sha = ".git" </> "objects" </> dir </> file
                    where (dir, file) = splitAt 2 sha

catFile :: String -> String -> IO()
catFile parameters shardName
    | parameters == "-p" = do
        result <- readObject shardName
        case result of
            Right content -> BL.putStr $ dropUnrelevant content
            Left e -> putStrLn $ "Error: " ++ show e
    | otherwise = putStrLn "Unknown Parameters for CatFile"


-- -- HashObject

addHeader :: BL.ByteString -> BL.ByteString
addHeader content = BL.append header content
  where
    header = C8.pack $ "blob " ++ show (BL.length content) ++ "\0"

hashObjectInternal :: FilePath ->  IO (Either IOException (Digest SHA1))
hashObjectInternal filePath = do
    content <- addHeader <$> BL.readFile filePath
    writeObjectFile content

hashObject :: FilePath -> IO()
hashObject filePath = hashObjectInternal filePath >>= print


-- -- Tree

data TreeEntry = TreeEntry {mode :: BL.ByteString, name :: BL.ByteString, sha ::  BL.ByteString} deriving (Show, Eq)
data TreeObject = TreeObject [TreeEntry]

lsTree :: Bool -> String -> IO()
lsTree nameOnly tree_sha = do
        treeObject <- readObject tree_sha
        case treeObject of
            Left err -> putStrLn $ "Error reading tree object: " ++ show err
            Right tree ->
                case parse parseTreeObject "" (unpack (BL.toStrict tree)) of
                    Left err -> putStrLn $ "Error parsing tree object: " ++ show err
                    Right tree -> displayEntity nameOnly (\entry -> entityIsFile entry || entityIsDirectory entry) tree

displayEntity :: Bool -> (TreeEntry -> Bool) -> TreeObject -> IO()
displayEntity nameOnly pred (TreeObject entries) =
        mapM_ (printEntry nameOnly) (filter pred entries)

printEntry :: Bool -> TreeEntry -> IO()
printEntry nameOnly entry =
    if nameOnly then putStrLn $ C8.unpack (name entry)
                else print entry

fileEntryModeValue :: ByteString
fileEntryModeValue = "100644"

directoryEntityModeValue :: ByteString
directoryEntityModeValue = "40000"

entityIsFile :: TreeEntry -> Bool
entityIsFile = (== fileEntryModeValue) . mode

entityIsDirectory :: TreeEntry -> Bool
entityIsDirectory = (== directoryEntityModeValue) . mode



parseTreeObject :: Parser TreeObject
parseTreeObject = do
    head <-    string "tree "
            *>  many1 digit
            *>  char '\0'
    entries <- many parseTreeEntry
    return $ TreeObject entries

--tree [content size]\0[Entries having references to other trees and blobs]
--[mode] [file/folder name]\0[SHA-1 of referencing blob or tree]
parseTreeEntry :: Parser TreeEntry
parseTreeEntry = do
    mode <- many1 digit
    char ' '
    name <- many1 (noneOf "\0")
    char '\0'
    sha <- count 20 anyChar
    return $ TreeEntry (C8.pack mode) (C8.pack name) (C8.pack sha)


-- write-tree
-- TreeEntry
--tree [content size]\0[Entries having references to other trees and blobs]
--[mode] [file/folder name]\0[SHA-1 of referencing blob or tree]
-- test again

digestToBL :: H.Digest a -> C8.ByteString
digestToBL = BL.fromStrict . BA.convert


isDir :: FilePath -> IO Bool
isDir = doesDirectoryExist

isFile :: FilePath -> IO Bool
isFile = doesFileExist

toByteStringRaw :: TreeObject -> ByteString
toByteStringRaw (TreeObject entries) = BL.concat $ map toByteStringRawEntry entries

toByteStringRawEntry :: TreeEntry -> ByteString
toByteStringRawEntry (TreeEntry mode name sha) = BL.concat [mode, " ", name, "\0", sha]

calculateContentSize :: TreeObject -> Int
calculateContentSize treeObject = fromIntegral $ BL.length $ toByteStringRaw treeObject

calculateContentSizeEntry :: TreeEntry -> Int
calculateContentSizeEntry entry = fromIntegral $ BL.length $ toByteStringRawEntry $ entry

addHeaderForTreeObject :: TreeObject -> ByteString
addHeaderForTreeObject treeObject = BL.append header (toByteStringRaw treeObject)
    where header = C8.pack $ "tree " ++ show (calculateContentSize treeObject) ++ "\0"

getTreeSha :: TreeObject -> Digest SHA1
getTreeSha treeObject = hashlazy $ addHeaderForTreeObject treeObject

createFileEntry :: FilePath -> Digest SHA1 -> IO TreeEntry
createFileEntry filePath sha = do
    let fileName = last $ split '/' (pack filePath)
    return $ TreeEntry  fileEntryModeValue (BL.fromStrict fileName) (digestToBL sha)

createDirectoryEntry :: FilePath -> Digest SHA1 -> IO TreeEntry
createDirectoryEntry filePath sha = do
    let dirName = last $ split '/' (pack filePath)
    return $ TreeEntry directoryEntityModeValue  (BL.fromStrict dirName) (digestToBL sha)

fetchDirectoryEntities :: [FilePath] -> IO [TreeEntry]
fetchDirectoryEntities filePaths = do
    filterM isDir filePaths >>= mapM createDirectoryEntityFromPath

fetchFileEntities :: [FilePath] -> IO [TreeEntry]
fetchFileEntities filePaths = do
    fileEntitysEithers <- filterM isFile filePaths >>=  mapM createFileEntityFromPath
    let fileEntitys = map (either (error . show) id) fileEntitysEithers
    return fileEntitys


createFileEntityFromPath :: FilePath -> IO (Either IOException TreeEntry)
createFileEntityFromPath filePath = do
    hash <- hashObjectInternal filePath
    traverse (createFileEntry filePath) hash

createDirectoryEntityFromPath :: FilePath -> IO TreeEntry
createDirectoryEntityFromPath filePath = do
    treeObject <- writeTreeInternal filePath
    let hash = getTreeSha treeObject
    createDirectoryEntry filePath hash

gitignore :: [FilePath]
gitignore = [".git", "git_test", ".direnv", "tags", ".stack-work"]

toEntity ::  FilePath -> IO (Maybe TreeEntry)
toEntity  filePath = isDir filePath >>= \case
    True -> Just <$> createDirectoryEntityFromPath filePath
    False -> isFile filePath >>= \case
        True -> Just <$> (createFileEntityFromPath filePath >>= either (error . show) return)
        False -> return Nothing

writeTreeInternal :: FilePath -> IO TreeObject
writeTreeInternal root = do
    filePath <- sort <$> (filter (`notElem` gitignore) <$> listDirectory root)
    treeObject <- TreeObject <$> getEntries (toEntity . withRootPath root) filePath
    writeTree treeObject
    return treeObject
        where
            withRootPath rootPath filePath = rootPath </> filePath
            getEntries method filepath = catMaybes <$> mapConcurrently method filepath
            writeTree treeObject = writeObjectFile $ addHeaderForTreeObject treeObject

writeTree :: FilePath -> IO ()
writeTree root = do
    treeObject <- writeTreeInternal root
    print $ getTreeSha treeObject


-- create commit
-- commit 156tree 070c6d5d918e5c051b5b8e72779b08c4eedd6da6
-- parent 4c4a4d4f2a9d3e9c1228a0940632663307c029ab
-- author John Doe <[email protected]> 1416224921 +0100
-- committer John Doe <[email protected]> 1416224921 +0100

-- This is my commit message

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

getTime ::IO BL.ByteString
getTime = C8.pack . DF.formatTime DF.defaultTimeLocale "%s %z" <$> DC.getCurrentTime

createCommitPerson :: IO CommitPerson
createCommitPerson = do
    let name = "Margaret Hamilton"
        email = "margaret.hamilton@NASA.com"
    CommitPerson name email <$> getTime

commitPersonToByteString :: CommitPerson -> BL.ByteString
commitPersonToByteString (CommitPerson name email timestamp) = BL.concat [name, " <", email, "> ", timestamp]

commitToByteString :: Commit -> BL.ByteString
commitToByteString (Commit treeSha parentSha author committer message) = 
    B.toLazyByteString $ mconcat [header, treeSHA, parentSHA, commitAuthor, currentCommitter, separator, commitMessage, separator]
    where
        separator = B.stringUtf8 "\n"
        header = B.stringUtf8 $ "commit " ++ show (BL.length message)
        treeSHA = B.stringUtf8 "tree " <> B.byteString treeSha <> separator
        parentSHA = B.byteString parentSha <> separator
        commitAuthor = B.stringUtf8 "author " <> B.byteString (commitPersonToByteString author) <> separator
        currentCommitter = B.stringUtf8 "committer " <> B.byteString (commitPersonToByteString committer) <> separator
        commitMessage = B.byteString message <> separator

-- what the fuck?
commitTreeInternal :: ByteString -> ByteString -> ByteString -> IO (Either IOException (Digest SHA1))
commitTreeInternal treeSha parentSha message = do
    let author = createCommitPerson
        committer = createCommitPerson
    commit <- commitToByteString <$> (Commit <$> pure treeSha <*> pure parentSha <*> author <*> committer <*> pure message)
    writeObjectFile commit

commitTree :: ByteString -> ByteString -> ByteString  -> IO ()
commitTree treeSha parentSha message = do 
    result <- commitTreeInternal treeSha parentSha message
    case result of
        Left err -> putStrLn $ "Error writing commit object: " ++ show err
        Right sha -> print sha

-- Main
main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO()
parseArgs ["init"] = initGitFile
parseArgs ["cat-file", parameters, blob_sha] = catFile parameters blob_sha
parseArgs ["hash-object", parameters, filePath] = hashObject filePath
parseArgs ["ls-tree", parameters, tree_sha]
        | parameters == "--name-only" = lsTree True tree_sha
        | otherwise = lsTree False tree_sha

parseArgs ["write-tree", root] = writeTree root
parseArgs ["write-tree"] = writeTree "."
-- i know i should using parser here instead of doing this, but let's just do it for now adas
parseArgs ["commit-tree", tree_sha, parameterA, parent_sha, parameterB,  message] = commitTree (C8.pack tree_sha) (C8.pack parent_sha) (C8.pack message)
parseArgs otherArgs =  void $ putStrLn ("Unknown options" <> show otherArgs)


-- helper

charToWord8 :: Char -> Word8
charToWord8 c = fromIntegral (ord c)

c2w :: Char -> Word8
c2w = fromIntegral . ord