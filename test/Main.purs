module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff (Aff)
import Text.Parsing.StringParser (Parser)
import Data.Either (Either(Right, Left))
import Data.Monoid (mempty)
import Data.String (trim)
import Main (isDownloaded, isBlacklisted, isProperName)
import Parser
  ( parse
  , tnode
  , tag
  , attribute
  , TagName(TagName)
  , Tag(TagOpen, TNode, TagClose, TagSingle)
  , Name(Name)
  , Value(Value)
  , Attribute(Attribute)
  , parseTags
  )
import Test.Unit (suite, test, failure)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

testHtml :: String
testHtml = """
<!DOCTYPE html>
<!-- whatever -->
<table>
  <tr>
    <td>Trash</td>
    <td class="target">
      <a href="http://mylink">
        [悪因悪果] 今季のゴミ - 01 [140p].avi
      </a>
    </td>
  </tr>
</table>
"""

expectedShowTestHtml :: String
expectedShowTestHtml = trim """
(Cons Parser.TagOpen (Parser.TagName "!DOCTYPE") (Data.List.Cons (Parser.Attribute (Parser.Name "html") (Parser.Value "")) Data.List.Nil) (Cons Parser.TagOpen (Parser.TagName "table") Data.List.Nil (Cons Parser.TagOpen (Parser.TagName "tr") Data.List.Nil (Cons Parser.TagOpen (Parser.TagName "td") Data.List.Nil (Cons Parser.TNode "Trash" (Cons Parser.TagClose (Parser.TagName "td") (Cons Parser.TNode ">" (Cons Parser.TagOpen (Parser.TagName "td") (Data.List.Cons (Parser.Attribute (Parser.Name "class") (Parser.Value "target")) Data.List.Nil) (Cons Parser.TagOpen (Parser.TagName "a") (Data.List.Cons (Parser.Attribute (Parser.Name "href") (Parser.Value "http://mylink")) Data.List.Nil) (Cons Parser.TNode "[悪因悪果] 今季のゴミ - 01 [140p].avi" (Cons Parser.TagClose (Parser.TagName "a") (Cons Parser.TNode ">" (Cons Parser.TagClose (Parser.TagName "td") (Cons Parser.TNode ">" (Cons Parser.TagClose (Parser.TagName "tr") (Cons Parser.TNode ">" (Cons Parser.TagClose (Parser.TagName "table") (Cons Parser.TNode ">" Nil))))))))))))))))))
"""

testParser :: forall e a. (Show a, Eq a) =>
  Parser a ->
  String ->
  a ->
  Aff (console :: CONSOLE | e) Unit
testParser p s expected =
  case parse p s of
    Right x -> do
      assert "parsing worked:" $ x == expected
    Left e ->
      failure $ "parsing failed: " <> show e

main :: forall e. Eff (console :: CONSOLE, testOutput :: TESTOUTPUT | e) Unit
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
  suite "Parser" do
    test "tnode" $
      testParser tnode "a b c " $ TNode "a b c"
    test "attribute" $
      testParser attribute "abc=\"1223\"" $ Attribute (Name "abc") (Value "1223")
    test "tag close" $
      testParser tag "</crap>" $ TagClose (TagName "crap")
    test "tag single" $
      testParser tag "<crap/>" $ TagSingle (TagName "crap") mempty
    test "tag open" $
      testParser tag "<crap> " $ TagOpen (TagName "crap") mempty
    test "tag open with attr" $
      testParser tag "<crap a=\"sdf\"> " $ TagOpen (TagName "crap") (pure (Attribute (Name "a") (Value "sdf")))
    test "parseTags" do
      case parseTags testHtml of
        Right x -> do
          assert "this should work" $ show x == expectedShowTestHtml
        Left e -> do
          failure (show e)
  where
    blacklist = ["koira", "talo"]
    downloads = ["apple", "burger"]
    blacklisted = isBlacklisted blacklist
    downloaded = isDownloaded downloads
