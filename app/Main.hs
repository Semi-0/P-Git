{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE LambdaCase #-}

module Main  where
import Object
import BasicIO
import Parser
import System.Directory (createDirectoryIfMissing, listDirectory, doesFileExist, doesDirectoryExist)
import System.IO (IOMode (WriteMode), hPutStrLn, withFile, writeFile)
import Text.ParserCombinators.Parsec hiding (spaces)
import Crypto.Hash (hashlazy, Digest, SHA1, digestFromByteString)
import Control.Exception (IOException, try)
import Control.Monad (void, filterM, unless)
import System.Directory (createDirectoryIfMissing)
import System.Environment
import System.FilePath ((</>))
import System.FilePath (takeDirectory)
import Control.Monad (guard)
import qualified Data.ByteArray  as BA
import qualified Data.ByteString.Lazy.Char8 as C8

import Codec.Compression.Zlib (decompress, compress)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy  as BL
import qualified Data.ByteString as B
import qualified Crypto.Hash as H
import System.Directory (doesFileExist, doesDirectoryExist)
import Data.ByteString.Char8 (pack, unpack, split)

import Data.Word (Word8)
import Data.Char (ord)

import Control.Monad.RWS (MonadState(put, get))
import Data.Char (chr)
import GHC.IO.Device (RawIO(write))
import qualified Control.Exception as E
import Data.List (sort)
import Data.Maybe (catMaybes)
import Control.Concurrent.Async (mapConcurrently)
import qualified Data.Time as DF
import qualified Data.Time as DC
import qualified Data.ByteString.Builder as Builder

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




-- -- CatFile


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


lsTree :: Bool -> String -> IO()
lsTree nameOnly tree_sha = do
        treeObject <- readObject tree_sha
        case treeObject of
            Left err -> putStrLn $ "Error reading tree object: " ++ show err
            Right tree ->
                case parse parseTreeObject "" (unpack (BL.toStrict tree)) of
                    Left err -> putStrLn $ "Error parsing tree object: " ++ show err
                    Right tree -> displayEntity nameOnly (\entry -> entityIsFile entry || entityIsDirectory entry) tree


-- write-tree
-- TreeEntry
--tree [content size]\0[Entries having references to other trees and blobs]
--[mode] [file/folder name]\0[SHA-1 of referencing blob or tree]
-- test again

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
    Builder.toLazyByteString $ mconcat [header, treeSHA, parentSHA, commitAuthor, currentCommitter, separator, commitMessage]
    where
        separator = Builder.stringUtf8 "\n"
        header = Builder.stringUtf8 $ "commit " ++ show (BL.length message)  <> "\0"
        treeSHA = Builder.stringUtf8 "tree " <> Builder.byteString (BL.toStrict treeSha) <> separator
        parentSHA =  Builder.stringUtf8 "parent " <> Builder.byteString (BL.toStrict parentSha) <> separator
        commitAuthor = Builder.stringUtf8 "author " <> Builder.byteString (BL.toStrict $ commitPersonToByteString author) <> separator
        currentCommitter = Builder.stringUtf8 "committer " <> Builder.byteString (BL.toStrict $ commitPersonToByteString committer) <> separator
        commitMessage = Builder.byteString (BL.toStrict message) <> separator

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


-- Clone Respository
        

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


