module Main(main) where

import Directory
import Yawn.Data 
import Yawn.Server as Server

main :: IO()
main = do
  dir <- getCurrentDirectory
  Server.run $ Configuration 9000 "localhost" (dir ++ "/www")
