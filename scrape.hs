#!/usr/bin/env runghc
module Main where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import qualified System.Process as Proc

-- should only be one now, pages to scrape
newtype Url = Url String

-- base url for downloading links
newtype BaseUrl = BaseUrl String

-- shit i dont want to see
newtype BannedWord = BannedWord String

data Config = Config
  [Url]
  BaseUrl -- base url we might use
  [BannedWord]

-- file name
newtype Title = Title String

data Target = Target Title Url

strip :: String -> String
strip = Text.unpack . Text.strip . Text.pack

readSystem :: String -> [String] -> IO String
readSystem cmd args = do
  let process = Proc.proc cmd args
  (exitCode, out, err) <- Proc.readCreateProcessWithExitCode process ""
  case exitCode of
    Exit.ExitSuccess -> do
      pure . strip $ out
    _ -> do
      putStrLn $ "error while running " <> cmd <> ":"
      putStr err
      Exit.exitWith exitCode

readBash :: String -> IO String
readBash cmd = readSystem "bash" ["-c", cmd]

readConfig :: IO Config
readConfig = do
  baseUrl <- readBash "jq '.baseUrl' config.json -r"
  urls <- List.lines <$> readBash "jq '.urls | values[]' config.json -r"
  bannedWords <- List.lines <$> readBash "jq '.bannedWords | values[]' config.json -r"
  pure $ Config (Url <$> urls) (BaseUrl baseUrl) (BannedWord <$> bannedWords)

getTargets :: Config -> IO [Target]
getTargets (Config urls _ _) = do
  (=<<) id <$> traverse getTarget urls

getTarget :: Url -> IO [Target]
getTarget (Url srcUrl) = do
  _ <- readBash $ "curl '" <> srcUrl <> "' > scratch.crap"
  results <- readBash cmd
  pure . Maybe.mapMaybe readTarget . List.lines $ results
  where
    cmd =
      "content=$(cat scratch.crap | hxnormalize -x | hxselect 'td > a[title^=\\[][href^=\\/view]') && xml=\"<results>$content</results>\" && echo $xml | xsltproc pick.xsl -"

readTarget :: String -> Maybe Target
readTarget s
  | txt <- Text.pack s
  , first : second : _ <- Text.splitOn (Text.pack "||||||||||") txt
  , _ : title' : [] <- Text.splitOn (Text.pack "title:") first
  , _ : href' : [] <- Text.splitOn (Text.pack "href:") second
  , title <- Text.unpack title'
  , href <- Text.unpack href'
    = Just $ Target (Title title) (Url href)
readTarget _ = Nothing

containsNumbers :: Target -> Bool
containsNumbers (Target (Title title) _) = innerSearch title
  where
    numbers = ['0','1','2','3','4','5','6','7','8','9']
    innerSearch str = case str of
      '-' : ' ' : a : b : _
        | elem a numbers
        , elem b numbers
        -> True
      _ : rest -> innerSearch rest
      [] -> False

isNotBlacklisted :: Config -> Target -> Bool
isNotBlacklisted (Config _ _ blacklist) (Target (Title title) _) =
  all noMatches blacklist
  where
    noMatches (BannedWord bannedWord) =
      not $ bannedWord `List.isInfixOf` title

data Download = Download DownloadPath Url

-- filepath for link downloads
newtype DownloadPath = DownloadPath String

getDownloads :: Config -> Target -> IO (Maybe Download)
getDownloads (Config _ (BaseUrl baseUrl) _) (Target (Title title) (Url url')) = do
  exists <- Dir.doesPathExist path
  exists2 <- Dir.doesPathExist $ path <> ".added"
  return $ if exists || exists2
    then Nothing
    else do
      Just $ Download (DownloadPath path) (Url url)
  where
    url = baseUrl <> url' <> "/torrent"
    path = "downloads/" <> title <> ".torrent"

downloadTarget :: Download -> IO ()
downloadTarget (Download (DownloadPath path) (Url url)) = do
  _ <- readBash $ "curl '" <> url <> "' -o \"" <> path <> "\""
  putStrLn $ "downloaded " <> path

main :: IO ()
main = do
  config <- readConfig
  allTargets <- getTargets config

  let
    allowed
      = List.filter (isNotBlacklisted config)
      . List.filter containsNumbers
      $ allTargets
  targets <- Maybe.catMaybes <$> traverse (getDownloads config) allowed
  case targets of
    [] -> putStrLn "nothing new to download"
    _ -> do
      _ <- traverse downloadTarget targets
      return ()
