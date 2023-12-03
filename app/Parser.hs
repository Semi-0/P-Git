module Parser where
import Text.ParserCombinators.Parsec hiding (spaces)
import Object
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString as BL
import Data.ByteArray.Parse (anyByte)



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

