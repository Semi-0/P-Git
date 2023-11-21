{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

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

import Codec.Compression.Zlib (decompress, compress)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy  as BL
import qualified Data.ByteString as B
import System.Directory (doesFileExist, doesDirectoryExist)
import Data.ByteString.Char8 (pack, unpack, split)
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Word (Word8)
import Data.Char (ord)
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.RWS (MonadState(put))
import Data.Char (chr)
import GHC.IO.Device (RawIO(write))
import qualified Control.Exception as E
import Data.List (sort)
import Data.Maybe (catMaybes)


-- Init


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

-- Basic IO

-- readObject :: String -> IO ByteString
-- readObject blob_sha = do
--                         let filePath = appendPath blob_sha
--                         fileExists <- doesFileExist filePath
--                         if fileExists
--                             then do
--                                 file_content <- BL.readFile filePath
--                                 return $ decompress file_content
--                             else return $ "File does not exist"

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
        fileDir = appendPath (show sha)
    createDirectoryIfMissing True (takeDirectory fileDir)
    E.try $ B.writeFile fileDir (BL.toStrict $ compress content) >> return sha

-- CatFile

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

-- HashObject

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






-- Tree

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

data TreeEntry = TreeEntry {mode :: BL.ByteString, name :: BL.ByteString, sha ::  BL.ByteString} deriving (Show, Eq)
data TreeObject = TreeObject [TreeEntry]

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

isDir :: FilePath -> IO Bool
isDir = doesDirectoryExist

isFile :: FilePath -> IO Bool
isFile = doesFileExist

toByteStringRaw :: TreeObject -> ByteString
toByteStringRaw (TreeObject entries) = BL.concat $ map toByteStringRawEntry entries

toByteStringRawEntry :: TreeEntry -> ByteString
toByteStringRawEntry (TreeEntry mode name sha) = BL.concat [mode, " ", name, "\0", sha]

calculateContentSize :: TreeObject -> Int
calculateContentSize (TreeObject entries) = sum $ map calculateContentSizeEntry entries

calculateContentSizeEntry :: TreeEntry -> Int
calculateContentSizeEntry (TreeEntry mode name sha) = 1 + fromIntegral (BL.length mode) + 1 + fromIntegral (BL.length name) + 1 + fromIntegral (BL.length sha)

addHeaderForTreeObject :: TreeObject -> ByteString
addHeaderForTreeObject treeObject = BL.append header (toByteStringRaw treeObject)
    where header = C8.pack $ "tree " ++ show (calculateContentSize treeObject) ++ "\0"



getTreeSha :: TreeObject -> Digest SHA1
getTreeSha treeObject = hashlazy $ addHeaderForTreeObject treeObject

createFileEntry :: FilePath -> Digest SHA1 -> IO TreeEntry
createFileEntry filePath sha = do
    let fileName = last $ split '/' (pack filePath)
    return $ TreeEntry  fileEntryModeValue (BL.fromStrict fileName) (C8.pack $ show sha)

createDirectoryEntry :: FilePath -> Digest SHA1 -> IO TreeEntry
createDirectoryEntry filePath sha = do
    let dirName = last $ split '/' (pack filePath)
    return $ TreeEntry directoryEntityModeValue  (BL.fromStrict dirName) (C8.pack $ show sha)

createFileEntityFromPath :: FilePath -> IO (Either IOException TreeEntry)
createFileEntityFromPath filePath = do
    hash <- hashObjectInternal filePath
    traverse (createFileEntry filePath) hash


createDirectoryEntityFromPath :: FilePath -> IO TreeEntry
createDirectoryEntityFromPath filePath = do
    treeObject <- writeTreeInternal filePath
    let hash = getTreeSha treeObject
    createDirectoryEntry filePath hash

fetchDirectoryEntities :: [FilePath] -> IO [TreeEntry]
fetchDirectoryEntities filePaths = do
    filterM isDir filePaths >>= mapM createDirectoryEntityFromPath

fetchFileEntities :: [FilePath] -> IO [TreeEntry]
fetchFileEntities filePaths = do
    fileEntitysEithers <- filterM isFile filePaths >>=  mapM createFileEntityFromPath
    let fileEntitys = map (either (error . show) id) fileEntitysEithers
    return fileEntitys


gitignore :: [FilePath]
gitignore = [".git", "git_test", ".direnv", "tags", ".stack-work"]


toEntity :: FilePath -> IO (Maybe TreeEntry)
toEntity filePath = do
    isDirectory <- isDir filePath
    if isDirectory
    then Just <$> createDirectoryEntityFromPath filePath
    else do
        isFile <- doesFileExist filePath
        if isFile
        then createFileEntityFromPath filePath >>= either (error . show) (return . Just)
        else return Nothing

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = fmap catMaybes . mapM f

writeTreeInternal :: FilePath -> IO TreeObject
writeTreeInternal root = do
    filePaths <- listDirectory root
    let processedFilePath = sort $ filter (`notElem` gitignore) filePaths
    treeEntries <- mapMaybeM  toEntity processedFilePath
    let treeObject = TreeObject treeEntries
    let content = addHeaderForTreeObject treeObject
    writeObjectFile content
    return treeObject

writeTree :: FilePath -> IO ()
writeTree root = do
    treeObject <- writeTreeInternal root
    print $ getTreeSha treeObject

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
parseArgs otherArgs =  void $ putStrLn ("Unknown options" <> show otherArgs)

-- helper

charToWord8 :: Char -> Word8
charToWord8 c = fromIntegral (ord c)

c2w :: Char -> Word8
c2w = fromIntegral . ord