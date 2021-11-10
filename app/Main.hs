module Main where

import Cli
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as T (readFile)
import Format
import System.Environment
import System.IO
import Options.Applicative

main :: IO ()
main = do
  cli <- execParser opts
  T.readFile (inFilePath cli) >>= (writeFile (outFilePath cli) . format cli) . T.unpack
