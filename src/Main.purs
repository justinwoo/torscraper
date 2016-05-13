module Main where

import Prelude
import Control.Monad.Aff (Aff, launchAff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array ((:))
import Data.Foldable (any, foldl)
import Data.String (contains)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff (readTextFile, readdir)
import Node.Path (FilePath)

type File = String
type BannedWords = Array String
type DownloadedFiles = Array File
type FetchedTargets = Array Target
type DownloadTargets = Array Target
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

processFile :: BannedWords -> DownloadedFiles -> Array Target -> Target -> Array Target
processFile bannedWords downloadedFiles targets target =
  if blacklisted || downloaded then
    targets
  else
    target : targets
  where
    name = target.name
    blacklisted = isBlacklisted bannedWords name
    downloaded = isDownloaded downloadedFiles name

getDownloadTargets :: BannedWords -> DownloadedFiles -> FetchedTargets -> DownloadTargets
getDownloadTargets bannedWords downloadedFiles fetchedTargets =
  foldl
    (processFile bannedWords downloadedFiles)
    []
    fetchedTargets

isBlacklisted :: BannedWords -> File -> Boolean
isBlacklisted bannedFiles file =
  any (\x -> contains x file) bannedFiles

isDownloaded :: DownloadedFiles -> File -> Boolean
isDownloaded downloadedFiles file =
  any (contains file) downloadedFiles

foreign import parseConfigFile :: String -> Config
foreign import configPath :: String
getConfig :: forall e. Aff (fs :: FS | e) Config
getConfig = parseConfigFile <$> readTextFile UTF8 configPath

foreign import downloadDir :: String
getDownloadedFiles :: forall e. Aff (fs :: FS | e) (Array FilePath)
getDownloadedFiles = readdir downloadDir

foreign import data HTTP :: !
foreign import scrapeHtml :: Selector -> HtmlBody -> FetchedTargets
foreign import getTargetsPage :: forall e. (HtmlBody -> Eff (http :: HTTP | e) Unit) -> Url -> Eff (http :: HTTP | e) Unit
getFetchedTargets :: forall e. Url -> Selector -> Aff (http :: HTTP | e) FetchedTargets
getFetchedTargets url selector = scrapeHtml selector <$> makeAff (\e s -> getTargetsPage s url)

foreign import kickOffDownloads :: forall e. DownloadTargets -> Eff (console :: CONSOLE | e) Unit
kickOffDownloads' :: forall e. DownloadTargets -> Aff (console :: CONSOLE | e) Unit
kickOffDownloads' = liftEff <<< kickOffDownloads

main :: forall e. Eff (err :: EXCEPTION, fs :: FS, http :: HTTP, console :: CONSOLE | e) Unit
main = launchAff $ do
  {url, selector, blacklist} <- getConfig
  downloadedFiles <- getDownloadedFiles
  fetchedTargets <- getFetchedTargets url selector

  kickOffDownloads' $ getDownloadTargets blacklist downloadedFiles fetchedTargets
