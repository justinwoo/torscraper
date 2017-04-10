module Test.Main where

import Prelude
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Main (isDownloaded, isBlacklisted, isProperName)
import Test.Unit (suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: forall e.
  Eff
    ( console :: CONSOLE
    , testOutput :: TESTOUTPUT
    , avar :: AVAR
    | e
    )
    Unit
main = runTest do
  suite "isProperName" do
    test "make sure it finds the proper name" do
      assert "this should work" $ isProperName "[Du] Lelunallella - 01 [720p].mkv" == true
      assert "should not work" $ isProperName "[Gamla] on - Batch [720p].mkv" == false
      assert "should not work" $ isProperName "[Du] paljon (01-12) [720p].mkv" == false
      assert "should not work" $ isProperName "[Fria] sisua [720p].mkv" == false
  suite "isBlacklisted" do
    test "make sure it blacklists shit" do
      assert "should not work" $ blacklisted "porkkanapiirakka" == false
      assert "this should work" $ blacklisted "mustakoira" == true
      assert "this should work" $ blacklisted "makkaratalo" == true
  suite "isDownloaded" do
    test "make sure it marks shit" do
      assert "should not work" $ downloaded "pizza" == false
      assert "this should work" $ downloaded "burger" == true
      assert "this should work" $ downloaded "apple" == true
  where
    blacklist = ["koira", "talo"]
    downloads = ["apple", "burger"]
    blacklisted = isBlacklisted blacklist
    downloaded = isDownloaded downloads
