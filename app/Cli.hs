module Cli
  ( processCli,
    CliOpts (..),
    opts,
  )
where

import Data.Maybe
import Data.Semigroup ((<>))
import Options.Applicative
import Text.Read

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

data CliOpts = CliOpts
  { inFilePath :: String,
    outFilePath :: String,
    indentationLevel :: Int,
    maxLineLnegth :: Int,
    useStartBullet :: Bool
  }
  deriving (Eq)

versionStr :: String
versionStr = "0.1.0"

processCli :: Parser CliOpts
processCli =
  CliOpts
    <$> strOption
      ( long "input-file"
          <> short 'f'
          <> metavar "FILE"
          <> help "Path to input file"
      )
    <*> strOption
      ( long "output-file"
          <> short 'o'
          <> metavar "FILE"
          <> help "Path to output file"
      )
    <*> option
      auto
      ( long "indentation"
          <> short 'i'
          <> metavar "N"
          <> value 2
          <> showDefault
          <> help "Indentation level"
      )
    <*> option
      auto
      ( long "max-line-length"
          <> short 'l'
          <> metavar "N"
          <> value 80
          <> showDefault
          <> help "Maximum length of a line before it gets wrapped"
      )
    <*> switch
      ( long "use-star"
          <> short 's'
          <> showDefault
          <> help "Use star/asterisk/* for bullet points instead of dash"
      )

opts =
  info
    (processCli <**> helper)
    ( fullDesc
        <> header "Formatter for markdown"
        <> progDesc "Formats a markdown file"
    )
