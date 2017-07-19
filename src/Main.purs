module Main where

import Prelude
import Control.Monad.Eff.Exception as Exc
import Data.Array as A
import Control.Monad.Aff (Aff, Canceler, launchAff, makeAff)
import Control.Monad.Aff.Console (error, log)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Control.Monad.Except (runExcept)
import Data.Array (filter)
import Data.Either (Either(..), fromRight)
import Data.Foldable (any, find, foldMap, traverse_)
import Data.Foreign (F)
import Data.Foreign.Class (class Decode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, genericDecode)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Generic.Rep (class Generic)
import Data.List (fromFoldable, (:))
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(Pattern), contains)
import Data.String.HtmlElements (decode)
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Global.Unsafe (unsafeStringify)
import LenientHtmlParser (Attribute(..), Name(..), Tag(..), TagName(..), Value(..), parseTags)
import Node.ChildProcess (CHILD_PROCESS, defaultSpawnOptions, onClose, onError, spawn)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (FS, readTextFile, readdir)
import Node.Path (concat)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (ParseError)

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
instance decodeUrl :: Decode Url where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

type HtmlBody = String

newtype Config = Config
  { urls :: Array Url
  , baseUrl :: Url
  , blacklist :: BannedWords
  }
derive instance genericConfig :: Generic Config _
instance decodeConfig :: Decode Config where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

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

getConfig :: forall a e.
  Decode a
  => Aff
       ( fs :: FS
       | e
       )
       (F a)
getConfig = decodeJSON <$> readTextFile UTF8 "./config.json"

getDownloadedFiles :: forall e.
  Aff
    ( fs :: FS
    | e
    )
    (Array FilePath)
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

getFetchedTargets :: forall e.
  Url ->
  Aff (ajax :: AJAX | e) (P FetchedTargets)
getFetchedTargets url = do
  res <- get url
  pure $ FetchedTargets <$> scrapeHtml res

foreign import data AJAX :: Effect
foreign import ajaxGet :: forall e.
  Fn3
    String
    (Error -> Eff (ajax :: AJAX | e) Unit)
    (String -> Eff (ajax :: AJAX | e) Unit)
    (Eff (ajax :: AJAX | e) Unit)
get :: forall e.
  Url
  -> Aff
       ( ajax :: AJAX
       | e
       )
       String
get (Url url) = makeAff (\error success -> runFn3 ajaxGet url error success)

type MyEffects e =
  ( fs :: FS
  , ajax :: AJAX
  , console :: CONSOLE
  , cp :: CHILD_PROCESS
  | e
  )

curl :: forall e.
  String
  -> String
  -> Aff
      ( cp :: CHILD_PROCESS
      | e
      )
      Unit
curl url path = do
  cp <- liftEff $ spawn "curl" [url, "-o", path] defaultSpawnOptions
  makeAff \e s -> do
    onError cp (e <<< Exc.error <<< unsafeStringify)
    onClose cp (s <<< const unit)

downloadTarget :: forall e.
  Url
  -> Target
  -> Aff
       ( cp :: CHILD_PROCESS
       , console :: CONSOLE
       | e
       )
       Unit
downloadTarget (Url baseUrl) {url, name} = do
  _ <- curl targetUrl path
  log $ "downloaded " <> path
  where
    targetUrl =
      baseUrl
      <> url
      <> "/torrent"
    path = concat [downloadsPath, name <> ".torrent"]

scrape :: forall e.
  BannedWords
  -> DownloadedFiles
  -> Url
  -> Aff
      ( ajax :: AJAX
      , console :: CONSOLE
      | e
      )
      DownloadTargets
scrape blacklist files url = getFetchedTargets url >>= case _ of
  Right xs -> do
    pure $ getDownloadTargets blacklist files xs
  Left e -> do
    error $ "error from parsing html: " <> show e
    pure mempty

main :: forall e.
  Eff
    (MyEffects (exception :: EXCEPTION | e))
    (Canceler (MyEffects e))
main = launchAff $ do
  config <- getConfig
  case runExcept config of
    Right (Config {urls, baseUrl, blacklist}) -> do
      files <- getDownloadedFiles
      ys <- foldMap (scrape blacklist files) urls
      case unwrap $ ys of
        [] -> log "nothing new to download"
        targets -> traverse_ (downloadTarget baseUrl) targets
      pure unit
    Left e -> error $ show e
