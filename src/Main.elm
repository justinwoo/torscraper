module Main where

import String

type alias File = String
type alias Target =
  {
    name : File,
    url : String
  }
type alias BannedWords = List File
type alias DownloadedFiles = List File
type alias FetchedTargets = List Target
type alias DownloadRequest = List Target
type alias GetDownloads = String

port bannedWordsSignal : Signal BannedWords
port downloadedFilesSignal : Signal DownloadedFiles
port fetchedTargetsSignal : Signal FetchedTargets
port getDownloadsSignal : Signal (Maybe String)

processFile : BannedWords -> DownloadedFiles -> Target -> List Target -> List Target
processFile bannedWords downloadedFiles target targets =
  let
    name = target.name
    blacklisted = isBlacklisted bannedWords name
    downloaded = isDownloaded downloadedFiles name
  in
    if | blacklisted || downloaded -> targets
       | otherwise -> target :: targets

getDownloadRequests : BannedWords -> DownloadedFiles -> FetchedTargets -> a -> DownloadRequest
getDownloadRequests bannedWords downloadedFiles fetchedTargets _ =
  List.foldl
    (processFile bannedWords downloadedFiles)
    []
    fetchedTargets

port requestDownloadsSignal : Signal DownloadRequest
port requestDownloadsSignal =
  Signal.map4
    getDownloadRequests
    bannedWordsSignal
    downloadedFilesSignal
    fetchedTargetsSignal
    getDownloadsSignal

isBlacklisted : BannedWords -> File -> Bool
isBlacklisted bannedFiles file =
  List.any (\x -> String.contains x file) bannedFiles

isDownloaded : DownloadedFiles -> File -> Bool
isDownloaded downloadedFiles file =
  List.any (\x -> String.contains file x) downloadedFiles
