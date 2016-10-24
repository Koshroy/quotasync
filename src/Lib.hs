{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( deleteFunc,
      removeFile,

      module ClassyPrelude,
      module FileSpace
    ) where

import ClassyPrelude
import Control.Lens
import Data.Aeson.Lens
import qualified Data.CaseInsensitive as CI (mk)
--import Network.HTTP.Base (urlEncodeVars)
import Network.Wreq
import System.DiskSpace
import System.Command
import System.Directory
import System.FilePath

import FileSpace


percentUsed :: Integer -> Integer -> Float
percentUsed availSpace totalSpace =
  (fromIntegral totalSpace - fromIntegral availSpace) / (fromIntegral totalSpace)


diskUsedPercentage :: FilePath -> IO Float
diskUsedPercentage path = do
  usage <- getDiskUsage $ pack path
  let available = diskAvail usage
  let total = diskTotal usage
  return $ percentUsed available total


dirFileSpaces :: Integer -> FilePath -> IO [FileSpace]
dirFileSpaces days path = do
  -- Find all files in the path, exclude directories, files were modified
  -- >= n days ago. Print the file lists out in format "fileSize\tfileName\n"
  let fileSpaceCmdStr =
        "find " ++ path ++ " -type f -mtime +" ++ (show days) ++ " -printf '%s\t%p\n'"
  output <- bracket
            (do
                (_, Just hOut, _, _) <- createProcess
                                        (shell fileSpaceCmdStr) { std_out = CreatePipe }
                return hOut
            )
            hClose
            hGetContents
  let lineSplitOutputLines = fmap (\l -> splitElem '\t' (l :: Text)) (lines output)
  let tuplizedLSOL = fmap (\lst ->
                             let firstLst = case headMay lst of
                                   Just s -> s
                                   Nothing -> "0"
                                 secondLst = case (tailMay lst >>= headMay) of
                                   Just p -> p
                                   Nothing -> "" in
                               (firstLst, secondLst)
                             ) lineSplitOutputLines
  return $ fmap ( \(szStr, fPath) ->
           case readMay szStr of
             Just num -> FileSpace { path = fPath, size = num }
             Nothing -> FileSpace { path = fPath, size = -1 }
       ) tuplizedLSOL


isFileDeletable :: FilePath -> IO Bool
isFileDeletable path = return False


deleteFile :: FilePath -> IO ()
deleteFile path = do
  isDeletable <- isFileDeletable path
  case isDeletable of
    True -> removeFile path
    False -> return ()


-- This function takes a list of candidate files to delete and returns the actual set of
-- files to delete. The algorithm simply takes the highest space files in the input
-- list and successively decrements the amount of available space until it falls below
-- the correct metric
filesToDelete ::
  DescendingFileSpaces -> Float -> Integer -> Integer -> (FilePath -> Bool) -> [FileSpace]
filesToDelete dFspaces wantedAvailPercent availSpace totalSpace canDeleteFunc =
  case (percentUsed availSpace totalSpace) <= (1 - wantedAvailPercent) of
    True -> []
    False ->
      let fspaces = spaces dFspaces in
        case headMay fspaces of
          Nothing -> []
          Just fspace -> let toDelFspace =
                               case canDeleteFunc (unpack $ path fspace) of
                                 False -> []
                                 True -> [fspace]
                         in
                           toDelFspace ++
                           (filesToDelete (tailSpaces dFspaces) wantedAvailPercent
                            (availSpace + size fspace) totalSpace canDeleteFunc)


deleteFunc :: Text -> Text -> Text -> Integer -> IO [FileSpace]
deleteFunc fileDir apiKey folderId port = do
  percentage <- diskUsedPercentage "/"
  usage <- getDiskUsage "/"
  fsps <- dirFileSpaces 7 (unpack fileDir)
  let apiKeyHeader = CI.mk $ asByteString $ encodeUtf8 "X-API-Key"
  let reqOpts =
        defaults
        & (param "folder" .~ [folderId])
        & (header apiKeyHeader .~ [encodeUtf8 apiKey])

  let portText = tshow port
  response <- getWith reqOpts $ unpack $ concat ["http://localhost:", portText, "/rest/db/need"]
  let progressFiles = response
        ^. responseBody
        ^.. key "progress" . _Array . traverse . to (\n -> n^? key "file") . _Just . _String
  let queuedFiles = response
        ^. responseBody
        ^.. key "progress" . _Array . traverse . to (\n -> n^? key "file") . _Just . _String
  let restFiles = response
        ^. responseBody
        ^.. key "progress" . _Array . traverse . to (\n -> n^? key "file") . _Just . _String
  let allUnsafeFiles = concat [progressFiles, queuedFiles, restFiles]
  let deletableFiles =
        filesToDelete (sortedFileSpaces fsps) 0.4
        (diskAvail usage) (diskTotal usage) (\e -> notElem (pack e :: Text) allUnsafeFiles)
  return deletableFiles
