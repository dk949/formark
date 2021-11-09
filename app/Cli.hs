module Cli
  ( processCli,
  )
where

import Data.Maybe
import Text.Read
import Types

type OptionsResult = Either String Options

defaultOpt :: Options
defaultOpt = Options Nothing Nothing 4 80 '*' 1

helpStr :: String
helpStr =
  unlines
    [ "formark -f FILE [-i N -l N -b C -n N -h -v]",
      "",
      "-f FILE      path to input file",
      "-o FILE      path to output file",
      "-i N         indentation level",
      "-l N         max line length",
      "-b C         bullet point character",
      "-n N         number of lines to insert between heading and text under heading",
      "-h           help",
      "-v           version"
    ]

versionStr :: String
versionStr = "0.1.0"

processCli :: [String] -> OptionsResult
processCli = parse defaultOpt

parse :: Options -> [String] -> OptionsResult
--
parse prevOpt ("-f" : path : rest) = parse (prevOpt {inFilePath = Just path}) rest
--
parse prevOpt ("-o" : path : rest) = parse (prevOpt {outFilePath = Just path}) rest
--
parse prevOpt ("-i" : num : rest) =
  case readMaybe num :: Maybe Int of
    Just n -> parse (prevOpt {indentationLevel = n}) rest
    Nothing -> Left ("-i expected a number as argument, got " ++ num)
--
parse prevOpt ("-l" : num : rest) =
  case readMaybe num :: Maybe Int of
    Just n -> parse (prevOpt {maxLineLnegth = n}) rest
    Nothing -> Left ("-l expected a number as argument, got " ++ num)
--
parse prevOpt ("-b" : ch : rest)
  | length ch == 1 && c == '*' || c == '-' = parse (prevOpt {bulletPointChar = c}) rest
  | otherwise = Left ("-b expected one of * or - got " ++ ch)
  where
    c = head ch
--
parse prevOpt ("-n" : num : rest) =
  case readMaybe num :: Maybe Int of
    Just n -> parse (prevOpt {numLinesUnderHeading = n}) rest
    Nothing -> Left ("-n expected a number as argument, got " ++ num)
--
parse prevOpt ("-h" : _) = Left helpStr
parse prevOpt ("-v" : _) = Left versionStr
parse prevOpt [badOpt] = Left ("Unrecognised option: " ++ badOpt ++ ". It is possible this option takes arguments")
parse prevOpt (badOpt : _) = Left ("Unrecognised option: " ++ badOpt)
parse prevOpt []
  | prevOpt == defaultOpt = Left "Expected arguments. Try -h for help"
  | otherwise = Right prevOpt
