module Main (..) where

import String
import Json.Decode exposing (Value)


type alias File =
  String


type alias Target =
  { name : File
  , url : String
  }


type alias BannedWords =
  List File


type alias DownloadedFiles =
  List File


type alias FetchedTargets =
  List Target


type alias DownloadRequest =
  List Target


port bannedWords : BannedWords
port downloadedFiles : DownloadedFiles
port fetchedTargets : FetchedTargets
port getDownloadsSignal : Signal Value
processFile : BannedWords -> DownloadedFiles -> Target -> List Target -> List Target
processFile bannedWords downloadedFiles target targets =
  let
    name =
      target.name

    blacklisted =
      isBlacklisted bannedWords name

    downloaded =
      isDownloaded downloadedFiles name
  in
    if blacklisted || downloaded then
      targets
    else
      target :: targets


getDownloadRequests : a -> DownloadRequest
getDownloadRequests _ =
  List.foldl
    (processFile bannedWords downloadedFiles)
    []
    fetchedTargets


port requestDownloadsSignal : Signal DownloadRequest
port requestDownloadsSignal =
  Signal.map
    getDownloadRequests
    getDownloadsSignal


isBlacklisted : BannedWords -> File -> Bool
isBlacklisted bannedFiles file =
  List.any (\x -> String.contains x file) bannedFiles


isDownloaded : DownloadedFiles -> File -> Bool
isDownloaded downloadedFiles file =
  List.any (\x -> String.contains file x) downloadedFiles

