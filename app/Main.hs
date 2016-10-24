{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Options.Applicative hiding ((<>))
import qualified Options.Applicative as OA ((<>))

data QuotaSyncArgs = QuotaSyncArgs
  { mediaDir :: String
  , apiKey :: String
  , folderId :: String
  , port :: Integer
  } deriving (Show)

argParser :: Parser QuotaSyncArgs
argParser = QuotaSyncArgs
  <$> strOption
      ( long "media_dir"
     OA.<> short 'm' )
  <*> strOption
      ( long "api_key"
     OA.<> short 'a' )
  <*> strOption
      ( long "folder_id"
     OA.<> short 'f' )
  <*> option auto
      ( long "port"
     OA.<> short 'p' )

removeFilePrintStatus :: FilePath -> IO ()
removeFilePrintStatus path = do
  putStrLn ("Deleting file: " ++ (tshow path))
  removeFile path


main :: IO ()
main = do
  conf <- execParser (info argParser fullDesc)
  fs <- deleteFunc
    (pack $ mediaDir conf) (pack $ apiKey conf) (pack $ folderId conf) (port conf)
  case null fs of
    True -> return ()
    False -> putStrLn $ ("Deleting files: " ++ (tshow $ (map tshow fs)))
  mapM_ (removeFilePrintStatus . asString . unpack . asText . path) fs
  threadDelay 5000000
  main
