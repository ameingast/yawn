module Yawn.Test.BlackBox.ParserTest where

import Test.HUnit
import Yawn.Test.Common

tests :: Test
tests = TestList [
  TestLabel "TestSimpleGet" testSimpleGet,
  TestLabel "TestSimplePost" testSimplePost]

testSimpleGet :: Test
testSimpleGet = TestCase $ do
  response <- transmit "GET / HTTP/1.1"
  assertEqual "GET /" "HTTP/1.1 200 Ok" response

testSimplePost :: Test
testSimplePost = TestCase $ do
  response <- transmit "POST / HTTP/1.1"
  assertEqual "POST /" "HTTP/1.1 200 Ok" response
