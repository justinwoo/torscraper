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
import Data.Array (null)
import Data.Either (Either(..), fromRight)
import Data.Foldable (any, find, traverse_)
import Data.Foreign (F)
import Data.Foreign.Class (class Decode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, genericDecode)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Generic.Rep (class Generic)
import Data.List (fromFoldable, (:))
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.String (Pattern(..), Replacement(..), contains, replace)
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
type FetchedTargets = Array Target
type DownloadTargets = Array Target
type FilePath = String
type Url = String
type HtmlBody = String

newtype Config = Config
  { url :: Url
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
getDownloadTargets bannedWords downloadedFiles fetchedTargets =
  fetchedTargets
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
getDownloadedFiles = readdir downloadsPath

scrapeHtml :: String -> Either ParseError (Array Target)
scrapeHtml text = do
  tags <- fromFoldable <$> parseTags text
  pure <<< A.fromFoldable $ extractTargets mempty tags
  where
    extractTargets acc tags =
      case tags of
        (TagOpen (TagName "td") _)
          : (TagOpen (TagName "a") attrs)
          : TNode s
          : xs -> case getHref attrs of
            Just url -> extractTargets ({name: decode s, url } : acc) xs
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
  pure $ scrapeHtml res

foreign import data AJAX :: Effect
foreign import ajaxGet :: forall e.
  Fn3
    Url
    (Error -> Eff (ajax :: AJAX | e) Unit)
    (String -> Eff (ajax :: AJAX | e) Unit)
    (Eff (ajax :: AJAX | e) Unit)
get :: forall e.
  String
  -> Aff
       ( ajax :: AJAX
       | e
       )
       String
get url = makeAff (\error success -> runFn3 ajaxGet url error success)

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
  Target
  -> Aff
       ( cp :: CHILD_PROCESS
       , console :: CONSOLE
       | e
       )
       Unit
downloadTarget {url, name} = do
  curl targetUrl path
  log $ "downloaded " <> path
  where
    targetUrl =
      replace (Pattern "view") (Replacement "download")
      <<< replace (Pattern "//") (Replacement "")
      $ url
    path = concat [downloadsPath, name <> ".torrent"]

main :: forall e.
  Eff
    (MyEffects (exception :: EXCEPTION | e))
    (Canceler (MyEffects e))
main = launchAff $ do
  config <- getConfig
  case runExcept config of
    Right (Config {url, blacklist}) -> do
      files <- getDownloadedFiles
      getFetchedTargets url >>=
        case _ of
          Right xs -> do
            let targets = getDownloadTargets blacklist files xs

            if null targets
              then log "nothing new to download"
              else traverse_ downloadTarget targets
          Left e -> do
            error $ "error from parsing html: " <> show e
    Left e -> error $ show e
