{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hPutStrLn, withFile)

import Control.Exception (IOException, try)
import Control.Monad (void)
import System.Directory (createDirectoryIfMissing)
import System.Environment
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hPutStrLn, withFile)
import Codec.Compression.Zlib (decompress)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy  as BL
import System.Directory (doesFileExist)
import Data.ByteString.Char8 (pack, unpack)


main :: IO ()
main = getArgs >>= parseArgs

dropUnrelevant :: ByteString -> ByteString
dropUnrelevant = BL.drop 8

doCatFile :: String -> IO()
doCatFile blob_sha = do 
                        let filePath = appendPath blob_sha
                        fileExists <- doesFileExist filePath
                        if fileExists
                            then do 
                                file_content <- BL.readFile filePath
                                BL.putStr $ dropUnrelevant $ decompress file_content
                            else putStrLn $ "File does not exist"
                       

appendPath :: String -> String 
appendPath filename = ".git" </> "objects" </> dir </> file
                    where (dir, file) = splitAt 2 filename


catFile :: String -> String -> IO() 
catFile parameters shardName 
    | parameters == "-p" = doCatFile shardName
    | otherwise = putStrLn "Unknown Parameters for CatFile"


initGitFile :: IO()
initGitFile = do 
    let createParents = True
    createDirectoryIfMissing createParents ".git"
    createDirectoryIfMissing createParents (".git" </> "objects")
    createDirectoryIfMissing createParents (".git" </> "refs")
    withFile (".git" </> "HEAD") WriteMode $ \f -> hPutStrLn f "ref: refs/heads/master"
    putStrLn $ "Initialized git directory"

parseArgs :: [String] -> IO() 
parseArgs ["init"] = initGitFile
parseArgs ["cat-file", parameters, blob_sha] = catFile parameters blob_sha
parseArgs otherArgs =  void $ putStrLn ("Unknown options" <> show otherArgs)

