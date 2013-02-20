module Yawn.Test.BlackBox.ParserTest where

import Test.HUnit
import Yawn.Test.Common

tests :: Test
tests = TestList [
  TestLabel "TestSimpleGet" testSimpleGet,
  TestLabel "TestSimplePost" testSimplePost,
  TestLabel "TestInvalidRequest" testInvalidRequest]

testSimpleGet :: Test
testSimpleGet = TestCase $ do
  response <- transmit "GET / HTTP/1.1"
  assertEqual "GET /" "HTTP/1.1 200 Ok" response
  response2 <- transmit "GET / HTTP/1.0"
  assertEqual "GET /" "HTTP/1.0 200 Ok" response2

testSimplePost :: Test
testSimplePost = TestCase $ do
  response <- transmit "POST / HTTP/1.1"
  assertEqual "POST /" "HTTP/1.1 200 Ok" response
  response2 <- transmit "POST / HTTP/1.0"
  assertEqual "POST /" "HTTP/1.0 200 Ok" response2
  
testInvalidRequest :: Test
testInvalidRequest = TestCase $ do
  response <- transmit "INVALID / HTTP/1.0"
  assertEqual "INVALID /" "HTTP/1.0 400 Bad Request" response