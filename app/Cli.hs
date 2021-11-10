module Cli
  ( CliOpts (..),
    opts,
    execParser,
  )
where

import Control.Applicative ((<**>))
import Data.Semigroup ((<>))
import Options.Applicative.Builder
import Options.Applicative.Extra (execParser, helper)
import Options.Applicative.Types (Parser)

data CliOpts = CliOpts
  { inFilePath :: String,
    outFilePath :: String,
    indentationLevel :: Int,
    maxLineLnegth :: Int,
    useDashBullet :: Bool
  }
  deriving (Eq)

processCli :: Parser CliOpts
processCli =
  CliOpts
    <$> strOption
      ( long "input-file"
          <> short 'f'
          <> metavar "INFILE"
          <> help "Path to input file"
      )
    <*> strOption
      ( long "output-file"
          <> short 'o'
          <> metavar "OUTFILE"
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
      ( long "use-dash"
          <> short 'd'
          <> showDefault
          <> help "Use dash (-) for bullet points instead of star/asterisk(*)"
      )

opts =
  info
    (processCli <**> helper)
    ( fullDesc
        <> header "Formatter for markdown"
        <> footer "For bug reports use https://github.com/dk949/formark/issues"
    )
