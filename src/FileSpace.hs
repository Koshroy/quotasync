{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module FileSpace
  (
    DescendingFileSpaces,
    FileSpace(..),
    spaces,
    sortedFileSpaces,
    tailSpaces
  ) where

import ClassyPrelude


data FileSpace = FileSpace {
  path :: Text
  , size :: Integer
  } deriving (Eq, Show)

instance Ord FileSpace where
  (FileSpace _ szA) `compare` (FileSpace _ szB) = szA `compare` szB


-- Data type making sure file spaces are sorted in descending
-- order
data DescendingFileSpaces = DescendingFileSpaces {
  spaces :: [FileSpace]
  } deriving (Eq, Show)

tailSpaces :: DescendingFileSpaces -> DescendingFileSpaces
tailSpaces (DescendingFileSpaces s) =
  case tailMay s of
    Nothing -> DescendingFileSpaces []
    Just t -> DescendingFileSpaces t


sortedFileSpaces :: [FileSpace] -> DescendingFileSpaces
sortedFileSpaces filespaces =
  DescendingFileSpaces $ sortOn (\(FileSpace p sz) -> FileSpace p (-1*sz)) filespaces
