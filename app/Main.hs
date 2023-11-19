{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hPutStrLn, withFile, writeFile)
import Crypto.Hash (hashlazy, Digest, SHA1, digestFromByteString)
import Control.Exception (IOException, try)
import Control.Monad (void)
import System.Directory (createDirectoryIfMissing)
import System.Environment
import System.FilePath ((</>))

import Codec.Compression.Zlib (decompress, compress)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy  as BL
import qualified Data.ByteString as B
import System.Directory (doesFileExist)
import Data.ByteString.Char8 (pack, unpack, split)
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Word (Word8)
import Data.Char (ord)
import Text.ParserCombinators.Parsec hiding (spaces)


-- CatFile

dropUnrelevant :: ByteString -> ByteString
dropUnrelevant = BL.drop 8

readObject :: String -> IO ByteString
readObject blob_sha = do 
                        let filePath = appendPath blob_sha
                        fileExists <- doesFileExist filePath
                        if fileExists
                            then do 
                                file_content <- BL.readFile filePath
                                return $ decompress file_content
                            else return $ "File does not exist"
                       

appendPath :: String -> String 
appendPath filename = ".git" </> "objects" </> dir </> file
                    where (dir, file) = splitAt 2 filename


catFile :: String -> String -> IO() 
catFile parameters shardName 
    | parameters == "-p" = do
        content <- readObject shardName
        BL.putStr $ dropUnrelevant content
        return ()
    | otherwise = putStrLn "Unknown Parameters for CatFile"

-- HashObject

addHeader :: BL.ByteString -> BL.ByteString
addHeader content = BL.append header content
  where
    header = C8.pack $ "blob " ++ show (BL.length content) ++ "\0"

writeObjectFile :: FilePath -> BL.ByteString -> IO ()
writeObjectFile filePath content = do
    let sha = hashlazy content :: Digest SHA1
        (dirName, name) = splitAt 2 $ show sha
        dir = ".git" </> "objects" </> dirName
    createDirectoryIfMissing True dir
    B.writeFile (dir </> name) (BL.toStrict $ compress content)
    putStrLn $ show sha


hashObject :: FilePath -> IO()
hashObject filePath = do
    content <- addHeader <$> BL.readFile filePath
    writeObjectFile filePath content

-- Init

initGitFile :: IO()
initGitFile = do 
    let createParents = True
    createDirectoryIfMissing createParents ".git"
    createDirectoryIfMissing createParents (".git" </> "objects")
    createDirectoryIfMissing createParents (".git" </> "refs")
    withFile (".git" </> "HEAD") WriteMode $ \f -> hPutStrLn f "ref: refs/heads/master"
    putStrLn $ "Initialized git directory"

-- Tree
lsTree :: Bool -> String -> IO()
lsTree nameOnly tree_sha = do 
                    treeObject <- readObject tree_sha
                    case parse parseTreeObject "" (unpack (BL.toStrict treeObject)) of
                        Left err -> putStrLn $ "Error parsing tree object: " ++ show err
                        Right tree -> displayEntity nameOnly (\entry -> isFile entry || isDirectory entry) tree


displayEntity :: Bool -> (TreeEntry -> Bool) -> TreeObject -> IO() 
displayEntity nameOnly pred (TreeObject entries) = 
        mapM_ (printEntry nameOnly) (filter pred entries)

printEntry :: Bool -> TreeEntry -> IO()
printEntry nameOnly entry = 
    if nameOnly then putStrLn (show (name entry))
                else print entry                                  

data TreeEntry = TreeEntry {mode :: BL.ByteString, name :: BL.ByteString, sha ::  BL.ByteString} deriving (Show, Eq)
data TreeObject = TreeObject [TreeEntry]

isFile :: TreeEntry -> Bool
isFile entry = mode entry == "100644"

isDirectory :: TreeEntry -> Bool
isDirectory entry = mode entry == "40000"

charToWord8 :: Char -> Word8
charToWord8 c = fromIntegral (ord c)

c2w :: Char -> Word8
c2w = fromIntegral . ord

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
parseArgs otherArgs =  void $ putStrLn ("Unknown options" <> show otherArgs)

