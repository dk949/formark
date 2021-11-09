module Main where

import Cli
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as T (readFile)
import Format
import System.Environment
import System.IO
import Types

main :: IO ()
main = do
  cli <- getArgs
  case processCli cli of
    Right opts -> case (inFilePath opts, outFilePath opts) of
      (Just inPath, Just outPath) -> T.readFile inPath >>= (writeFile outPath . format opts) . T.unpack
      _ -> hPutStrLn stderr "Expected path to file. Try -h for help"
    Left err -> hPutStrLn stderr err
