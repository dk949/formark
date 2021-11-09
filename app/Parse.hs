module Parse where

import Control.Applicative
import Control.Monad
import Data.Functor
import Text.ParserCombinators.Parsec hiding (many, (<|>))

data BulletT
  = Dash
  | Star
  | Numeric Int
  deriving (Eq, Show)

type BulletLevel = Int

toBullet :: String -> BulletT
toBullet "*" = Star
toBullet "-" = Dash
toBullet a = Numeric (read a)

data HeadT
  = H1
  | H2
  | H3
  | H4
  | H5
  | H6
  deriving (Enum, Show)

toEnum 1 = H1
toEnum 2 = H2
toEnum 3 = H3
toEnum 4 = H4
toEnum 5 = H5
toEnum a = H6

fromEnum H1 = 1
fromEnum H2 = 2
fromEnum H3 = 3
fromEnum H4 = 4
fromEnum H5 = 5
fromEnum H6 = 6

data MDValue
  = Head (HeadT, String)
  | Bullet (BulletT, BulletLevel, String)
  | PlainText String
  | CodeBlock String
  | IndentBlock String
  | MDFile [MDValue]
  deriving (Show)

mdHeadLevel :: Parser HeadT
mdHeadLevel = Parse.toEnum . length <$> some (char '#')

mdHead :: Parser MDValue
mdHead =
  Head
    <$> ( do
            level <- mdHeadLevel
            many $ char ' '
            plainText <- many $ noneOf "\n\r"
            return (level, plainText)
        )

mdBullet :: Parser MDValue
mdBullet =
  Bullet
    <$> ( do
            level <- many $ char ' '
            tp <- (: []) <$> oneOf ['-', '*'] <|> (many digit <* char '.')
            plainText <- many $ noneOf "\n\r"
            return (toBullet tp, length level, "hello")
        )

mdFile :: Parser [MDValue]
mdFile = many (mdValue <* oneOf "\n\r") <* eof

mdValue :: Parser MDValue
mdValue = mdHead <|> mdBullet

data JsonValue
  = B Bool
  | S String
  | A [JsonValue]
  | O [(String, JsonValue)]
  deriving (Show)

(<||>) :: Parser a -> Parser a -> Parser a
(<||>) x y = try x <|> y

ws :: Parser String
ws = many (oneOf " \n\t")

lexime :: Parser a -> Parser a
lexime p = p <* ws

matchTrue :: Parser JsonValue
matchTrue = B <$> lexime (string "true" $> True)

matchFalse :: Parser JsonValue
matchFalse = B <$> lexime (string "false" $> False)

stringLiteral :: Parser String
stringLiteral = char '"' *> many (noneOf ['"']) <* char '"'

jsonString :: Parser JsonValue
jsonString = lexime (S <$> stringLiteral)

array :: Parser JsonValue
array =
  A
    <$> ( lexime (char '[')
            *> (jsonValue `sepBy` lexime (char ','))
            <* lexime (char ']')
        )

objectEntry :: Parser (String, JsonValue)
objectEntry = do
  key <- stringLiteral
  char ':'
  value <- jsonValue
  return (key, value)

comma :: Parser Char
comma = char ','

object :: Parser JsonValue
object = O <$> (char '{' *> (objectEntry `sepBy` comma) <* char '}')

jsonValue :: Parser JsonValue
jsonValue = matchTrue <|> matchFalse <|> jsonString <|> array <|> object

mdString =
  unlines
    [ "# Some Words hi there Here come some more text",
      "* Bullet 1",
      "* Bullet 2",
      "- Bullet 3",
      "- Bullet 4",
      "1. Bullet 5",
      "2. Bullet 6",
      "3. Bullet 7",
      "    33. Bullet 8"
    ]

getParseMD :: String -> IO ()
getParseMD input = case parse mdFile "" input of
  Right p -> print p
  Left err -> print err
