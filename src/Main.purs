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
  
foreign import getConfig :: forall e. (Config -> Eff e Unit) -> Eff e Unit
getConfig' :: forall e. Aff e Config
getConfig' = makeAff (\e s -> getConfig s)

foreign import getDownloadedFiles :: forall e. (DownloadedFiles -> Eff e Unit) -> Eff e Unit
getDownloadedFiles' :: forall e. Aff e DownloadedFiles
getDownloadedFiles' = makeAff (\e s -> getDownloadedFiles s)

foreign import getFetchedTargets :: forall e. (FetchedTargets -> Eff e Unit) -> Config -> Eff e Unit
getFetchedTargets' :: forall e. Config -> Aff e FetchedTargets
getFetchedTargets' config = makeAff (\e s -> getFetchedTargets s config)

foreign import kickOffDownloads :: forall e. DownloadTargets -> Eff (console :: CONSOLE | e) Unit
kickOffDownloads' :: forall e. DownloadTargets -> Aff (console :: CONSOLE | e) Unit
kickOffDownloads' = liftEff <<< kickOffDownloads

main :: forall e. Eff (err :: EXCEPTION, console :: CONSOLE | e) Unit
main = launchAff $ do
  config <- getConfig'
  downloadedFiles <- getDownloadedFiles'
  fetchedTargets <- getFetchedTargets' config

  kickOffDownloads' $ getDownloadTargets config.blacklist downloadedFiles fetchedTargets
