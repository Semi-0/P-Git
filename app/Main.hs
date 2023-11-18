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

main :: IO ()
main = getArgs >>= parseArgs

dropUnrelevant :: ByteString -> ByteString
dropUnrelevant = BL.drop 8

doCatFile :: String -> ByteString
doCatFile blob_sha = do 
                        file_content <- BL.readFile . appendPath . blob_sha
                        dropUnrelevant $ decompress file_content
                       

appendPath :: String -> String 
appendPath filename = ".git" </> "objects" </> filename


CatFile :: String -> String -> IO() 
CatFile parameters shardName 
    | parameters == "-p" = do
        fileExists <- doesFileExist (appendPath shardName)
        if fileExists
            then putStrLn . doCatFile shardName
            else putStrLn "File does not exist"
    | otherWise = putStrLn "Unknown Parameters for CatFile"


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
parseArgs ["cat-file", parameters, blob_sha] = CatFile parameters blob_sha
parseArgs otherArgs =  void $ putStrLn ("Unknown options" <> show args)

