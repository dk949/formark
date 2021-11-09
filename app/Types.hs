module Types
  ( Options (..),
    FileContents,
  )
where

data Options = Options
  { inFilePath :: Maybe String,
    outFilePath :: Maybe String,
    indentationLevel :: Int,
    maxLineLnegth :: Int,
    bulletPointChar :: Char,
    numLinesUnderHeading :: Int
  }
  deriving (Eq)

type FileContents = String
