module Main(main) where

import Directory
import Yawn.Configuration
import Yawn.Server as Server

main :: IO()
main = do
  dir <- getCurrentDirectory
  Server.start $ Configuration 9000 "localhost" (dir ++ "/www") "index.html"
