module Main where

import String
import Debug exposing (log)

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

port bannedWordsSignal : Signal BannedWords
port downloadedFilesSignal : Signal DownloadedFiles
port fetchedTargetsSignal : Signal FetchedTargets

processFile : BannedWords -> DownloadedFiles -> Target -> List Target -> List Target
processFile bannedWords downloadedFiles target targets =
  let
    name = target.name
    blacklisted = isBlacklisted bannedWords name
    downloaded = isDownloaded downloadedFiles name
  in
    if | blacklisted || downloaded -> targets
       | otherwise -> target :: targets

getDownloadRequests : BannedWords -> DownloadedFiles -> FetchedTargets -> DownloadRequest
getDownloadRequests bannedWords downloadedFiles fetchedTargets =
  List.foldl
    (processFile bannedWords downloadedFiles)
    []
    fetchedTargets

port testSignal : Signal BannedWords
port testSignal = bannedWordsSignal

port requestDownloadSignal : Signal DownloadRequest
port requestDownloadSignal =
  Signal.map3
    getDownloadRequests
    bannedWordsSignal
    downloadedFilesSignal
    fetchedTargetsSignal

isBlacklisted : BannedWords -> File -> Bool
isBlacklisted bannedFiles file =
  List.any (\x -> String.contains x file) bannedFiles

isDownloaded : DownloadedFiles -> File -> Bool
isDownloaded downloadedFiles file =
  List.any (\x -> String.contains file x) downloadedFiles
