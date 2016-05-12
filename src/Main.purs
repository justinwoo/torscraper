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
type Config =
  { url :: String
  , selector :: String
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

foreign import getFetchedTargets :: forall e. (FetchedTargets -> Eff e Unit) -> Config -> Eff e Unit
getFetchedTargets' :: forall e. Config -> Aff e FetchedTargets
getFetchedTargets' config = makeAff (\e s -> getFetchedTargets s config)

foreign import kickOffDownloads :: forall e. DownloadTargets -> Eff (console :: CONSOLE | e) Unit
kickOffDownloads' :: forall e. DownloadTargets -> Aff (console :: CONSOLE | e) Unit
kickOffDownloads' = liftEff <<< kickOffDownloads

main :: forall e. Eff (err :: EXCEPTION, fs :: FS, console :: CONSOLE | e) Unit
main = launchAff $ do
  config <- getConfig
  downloadedFiles <- getDownloadedFiles
  fetchedTargets <- getFetchedTargets' config

  kickOffDownloads' $ getDownloadTargets config.blacklist downloadedFiles fetchedTargets
