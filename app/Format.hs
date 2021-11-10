module Format where

import Cli (CliOpts (..))
import Data.Default (def)
import Data.Text as T (pack, unpack)
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Options
  ( extensionsFromList,
    githubMarkdownExtensions,
    readerExtensions,
    writerColumns,
    writerExtensions,
    writerTabStop,
  )
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.Markdown (writeMarkdown)
import Text.Regex (mkRegex, subRegex)

dashRegex = mkRegex "^( *)-"

dashReplacer :: String -> String
dashReplacer str = subRegex dashRegex str "\\1*"

--mdExtentionList = extensionsFromList [Ext_backtick_code_blocks]

format :: CliOpts -> String -> String
format opts file =
  let result =
        readMarkdown def {readerExtensions = githubMarkdownExtensions} (T.pack file)
          >>= writeMarkdown
            def
              { writerColumns = lineLength,
                writerTabStop = indentationLevel opts,
                writerExtensions = githubMarkdownExtensions
              }
        where
          lineLength = case maxLineLnegth opts of
            0 -> maxBound :: Int
            other -> other
   in case runPure result of
        Right t -> (fixes . T.unpack) t
          where
            fixes = if useDashBullet opts then id else dashReplacer
        Left _ -> file
