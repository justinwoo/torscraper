module Main where

import Prelude

import Control.Monad.Aff (Canceler, Aff, launchAff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array ((:))
import Data.Either (fromRight)
import Data.Foldable (any)
import Data.Monoid (mempty)
import Data.String (contains)
import Data.String.Regex (regex, test, noFlags)
import Partial.Unsafe (unsafePartial)

type File = String
type BannedWords = Array String
type DownloadedFiles = Array File
type FetchedTargets = Array Target
type DownloadTargets = Array Target
type FilePath = String
type Url = String
type Selector = String
type HtmlBody = String
type Config =
  { url :: Url
  , selector :: Selector
  , blacklist :: BannedWords
  }
type Target =
  { name :: File
  , url :: String
  }

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
  any (flip contains file) bannedFiles

isDownloaded :: DownloadedFiles -> File -> Boolean
isDownloaded downloadedFiles file =
  any (contains file) downloadedFiles

foreign import data FS :: !
foreign import parseConfigFile :: String -> Config
foreign import configPath :: String
foreign import readTextFile :: forall e. (String -> Eff (fs :: FS | e) Unit) -> String -> Eff (fs :: FS | e) Unit
readTextFile' :: forall e. String -> Aff (fs :: FS | e) String
readTextFile' x = makeAff (\e s -> readTextFile s x)
getConfig :: forall e. Aff (fs :: FS | e) Config
getConfig = parseConfigFile <$> readTextFile' configPath

foreign import downloadDir :: String
foreign import readdir :: forall e. (Array FilePath -> Eff (fs :: FS | e) Unit) -> String -> Eff (fs :: FS | e) Unit
readdir' :: forall e. String -> Aff (fs :: FS | e) (Array FilePath)
readdir' x = makeAff (\e s -> readdir s x)
getDownloadedFiles :: forall e. Aff (fs :: FS | e) (Array FilePath)
getDownloadedFiles = readdir' downloadDir

foreign import data HTTP :: !
foreign import scrapeHtml :: Selector -> HtmlBody -> FetchedTargets
foreign import getTargetsPage :: forall e. (HtmlBody -> Eff (http :: HTTP | e) Unit) -> Url -> Eff (http :: HTTP | e) Unit
getFetchedTargets :: forall e. Url -> Selector -> Aff (http :: HTTP | e) FetchedTargets
getFetchedTargets url selector = scrapeHtml selector <$> makeAff (\e s -> getTargetsPage s url)

foreign import kickOffDownloads :: forall e. DownloadTargets -> Eff (console :: CONSOLE | e) Unit
kickOffDownloads' :: forall e. DownloadTargets -> Aff (console :: CONSOLE | e) Unit
kickOffDownloads' = liftEff <<< kickOffDownloads

type MyEffects e =
  ( fs :: FS
  , http :: HTTP
  , console :: CONSOLE
  | e
  )

main :: forall e.
  Eff
    (MyEffects (err :: EXCEPTION | e))
    (Canceler (MyEffects e))
main = launchAff $ do
  {url, selector, blacklist} <- getConfig
  downloadedFiles <- getDownloadedFiles
  fetchedTargets <- getFetchedTargets url selector

  kickOffDownloads' $ getDownloadTargets blacklist downloadedFiles fetchedTargets
