module Main where

import Cli
import Data.Text as T (unpack)
import Data.Text.IO as TIO (readFile)
import Format

main :: IO ()
main = do
  cli <- execParser opts
  TIO.readFile (inFilePath cli) >>= (writeFile (outFilePath cli) . format cli) . T.unpack
