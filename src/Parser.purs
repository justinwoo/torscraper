module Parser where

import Prelude
import Control.Alt ((<|>))
import Data.Either (Either)
import Text.Parsing.StringParser
  ( Parser
  , ParseError
  , runParser
  , fail
  )
import Text.Parsing.StringParser.String
  ( string
  , char
  , noneOf
  , satisfy
  , skipSpaces
  )
import Text.Parsing.StringParser.Combinators
  ( many
  , many1
  )
import Data.Generic (class Generic, gShow)
import Data.List (List, toUnfoldable)
import Data.Monoid (mempty)
import Data.String (trim, fromCharArray)

newtype TagName = TagName String
derive instance eqTagName :: Eq TagName
derive instance genericTagName :: Generic TagName
instance showTagName :: Show TagName where
  show = gShow

type Attributes = List Attribute

newtype Name = Name String
derive instance eqName :: Eq Name
derive instance genericName :: Generic Name
instance showName :: Show Name where
  show = gShow

newtype Value = Value String
derive instance eqValue :: Eq Value
derive instance genericValue :: Generic Value
instance showValue :: Show Value where
  show = gShow

data Attribute = Attribute Name Value
derive instance eqAttribute :: Eq Attribute
derive instance genericAttribute :: Generic Attribute
instance showAttribute :: Show Attribute where
  show = gShow

data Tag
  = TagOpen TagName Attributes
  | TagSingle TagName Attributes
  | TagClose TagName
  | TNode String
derive instance eqTag :: Eq Tag
derive instance genericTag :: Generic Tag
instance showTag :: Show Tag where
  show = gShow

flattenChars :: List Char -> String
flattenChars = fromCharArray <<< toUnfoldable

comment :: Parser Unit
comment =
  string "<!" *> innerComment
  where
    innerComment = do
      (many1 $ satisfy ((/=) '>'))
      *> char '>'
      *> pure unit

skipSpace :: Parser Unit
skipSpace =
  comment
  <|> skipSpaces
  <|> pure unit

lexeme :: forall p. Parser p -> Parser p
lexeme p = p <* skipSpace

validNameString :: Parser String
validNameString =
  trim <<< flattenChars
  <$> (many1 $ noneOf ['=', ' ', '<', '>', '/', '"'])

attribute :: Parser Attribute
attribute = lexeme $ do
  name <- validNameString
  value <- getValue <|> pure ""
  pure $ Attribute (Name name) (Value value)
  where
    getValue = do
      void $ string "=\""
      value <- trim <<< flattenChars <$> (many1 $ noneOf ['"'])
      void $ char '"'
      pure $ value

tagOpenOrSingleOrClose :: Parser Tag
tagOpenOrSingleOrClose = lexeme $
  char '<'
  *> (closeTag <|> tagOpenOrSingle)

closeTag :: Parser Tag
closeTag = lexeme $ do
  void $ char '/'
  name <- validNameString
  pure $ TagClose (TagName name)

tagOpenOrSingle :: Parser Tag
tagOpenOrSingle = lexeme $ do
  tagName <- TagName <$> validNameString <* skipSpaces
  attrs <- many attribute <|> pure mempty
  let spec' = spec tagName attrs
  (closeTagOpen spec')
    <|> (closeTagSingle spec')
    <|> fail "no closure in sight for tag opening"
  where
    spec tagName attrs constructor =
      constructor tagName attrs
    closeTagOpen f =
      char '>' *> (pure $ f TagOpen)
    closeTagSingle f =
      string "/>" *> (pure $ f TagSingle)

tnode :: Parser Tag
tnode = lexeme $ do
  text <- flattenChars <$> (many1 $ satisfy ((/=) '<'))
  pure $ TNode (trim text)

tag :: Parser Tag
tag = lexeme do
  (tagOpenOrSingleOrClose <|> tnode)

tags :: Parser (List Tag)
tags = do
  skipSpace
  many tag

parse :: forall a. Parser a -> String -> Either ParseError a
parse p s = runParser p s

parseTags :: String -> Either ParseError (List Tag)
parseTags s = parse tags s
