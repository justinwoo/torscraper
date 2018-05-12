module Main where

import Prelude

import Data.Array (filter)
import Data.Array as A
import Data.Either (Either(..), fromRight)
import Data.Foldable (any, find, foldMap, traverse_)
import Data.Generic.Rep (class Generic)
import Data.List (fromFoldable, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(Pattern), contains)
import Data.String.HtmlElements (decode)
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff)
import Effect.Aff as Exc
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as EC
import Global.Unsafe (unsafeStringify)
import LenientHtmlParser (Attribute(..), Name(..), Tag(..), TagName(..), Value(..), parseTags)
import Milkis as M
import Milkis.Impl.Node (nodeFetch)
import Node.ChildProcess (defaultSpawnOptions, onClose, onError, spawn)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir)
import Node.Path (concat)
import Partial.Unsafe (unsafePartial)
import Simple.JSON (class ReadForeign, readJSON)
import Text.Parsing.StringParser (ParseError)

fetch :: M.Fetch
fetch = M.fetch nodeFetch

log :: forall e. MonadEffect e => String -> e Unit
log = liftEffect <<< EC.log

error :: forall e. MonadEffect e => String -> e Unit
error = liftEffect <<< EC.error

type File = String
type BannedWords = Array String
type DownloadedFiles = Array File
newtype FetchedTargets = FetchedTargets (Array Target)
newtype DownloadTargets = DownloadTargets (Array Target)
derive newtype instance semigroupDownloadTargets :: Semigroup DownloadTargets
derive newtype instance monoidDownloadTargets :: Monoid DownloadTargets
derive instance newtypeDownloadTargets :: Newtype DownloadTargets _

type FilePath = String
newtype Url = Url String
derive instance newtypeUrl :: Newtype Url _
derive instance genericUrl :: Generic Url _
derive newtype instance readForeignURL :: ReadForeign Url

type HtmlBody = String

newtype Config = Config
  { urls :: Array Url
  , baseUrl :: Url
  , blacklist :: BannedWords
  }
derive instance genericConfig :: Generic Config _
derive newtype instance readForeignConfig :: ReadForeign Config

type P a = Either ParseError a

type Target =
  { name :: File
  , url :: String
  }

downloadsPath :: String
downloadsPath = "./downloads"

getDownloadTargets :: BannedWords -> DownloadedFiles -> FetchedTargets -> DownloadTargets
getDownloadTargets bannedWords downloadedFiles (FetchedTargets fetchedTargets) =
  DownloadTargets $ fetchedTargets
    >>= processFile
      [ isBlacklisted bannedWords
      , isDownloaded downloadedFiles
      , not isProperName
      ]
  where
    processFile tests x =
      case any (\f -> f x.name) tests of
        true -> mempty
        false -> pure x

isProperName :: String -> Boolean
isProperName =
  test $ unsafePartial $ fromRight $ regex "\\[\\w*\\] .* - \\d* \\[.*\\]\\.mkv" noFlags

isBlacklisted :: BannedWords -> File -> Boolean
isBlacklisted bannedFiles file =
  any (flip contains file <<< Pattern) $ bannedFiles

isDownloaded :: DownloadedFiles -> File -> Boolean
isDownloaded downloadedFiles file =
  any (contains (Pattern file)) downloadedFiles

getDownloadedFiles :: Aff (Array FilePath)
getDownloadedFiles =
  filter (contains $ Pattern "torrent")
  <$> readdir downloadsPath

scrapeHtml :: String -> Either ParseError (Array Target)
scrapeHtml text = do
  tags <- fromFoldable <$> parseTags text
  pure <<< A.fromFoldable $ extractTargets mempty tags
  where
    extractTargets acc tags =
      case tags of
        (TagOpen (TagName "td") _)
          : (TagOpen (TagName "a") _)
          : (TagOpen (TagName "i") _)
          : _
          : _
          : _
          : (TagOpen (TagName "a") attrs)
          : TNode s
          : xs -> case getHref attrs of
            Just url -> extractTargets ({name: decode s, url: decode url } : acc) xs
            _ -> extractTargets acc xs
        (TagOpen (TagName "td") _)
          : (TagOpen (TagName "a") attrs)
          : TNode s
          : xs -> case getHref attrs of
            Just url -> extractTargets ({name: decode s, url: decode url } : acc) xs
            _ -> extractTargets acc xs
        (_ : xs) -> extractTargets acc xs
        mempty -> acc
    getHref attrs = do
      (Attribute _ (Value href)) <- find match attrs
      pure $ href
    match (Attribute (Name name) _) = name == "href"

getFetchedTargets :: Url -> Aff (P FetchedTargets)
getFetchedTargets (Url url) = do
  res <- M.text =<< fetch (M.URL url) M.defaultFetchOptions
  pure $ FetchedTargets <$> scrapeHtml res

curl :: String -> String -> Aff Unit
curl url path = do
  cp <- liftEffect $ spawn "curl" [url, "-o", path] defaultSpawnOptions
  makeAff \cb -> do
    onError cp (cb <<< Left <<< Exc.error <<< unsafeStringify)
    onClose cp (cb <<< Right <<< const unit)
    pure mempty

downloadTarget :: Url -> Target -> Aff Unit
downloadTarget (Url baseUrl) {url, name} = do
  _ <- curl targetUrl path
  log $ "downloaded " <> path
  where
    targetUrl =
      baseUrl
      <> url
      <> "/torrent"
    path = concat [downloadsPath, name <> ".torrent"]

scrape :: BannedWords -> DownloadedFiles -> Url -> Aff DownloadTargets
scrape blacklist files url = getFetchedTargets url >>= case _ of
  Right xs -> do
    pure $ getDownloadTargets blacklist files xs
  Left e -> do
    error $ "error from parsing html: " <> show e
    pure mempty

main :: Effect Unit
main = launchAff_ $ do
  config <- readJSON <$> readTextFile UTF8 "./config.json"
  case config of
    Right (Config {urls, baseUrl, blacklist}) -> do
      files <- getDownloadedFiles
      ys <- foldMap (scrape blacklist files) urls
      case unwrap $ ys of
        [] -> log "nothing new to download"
        targets -> traverse_ (downloadTarget baseUrl) targets
      pure unit
    Left e -> error $ show e
