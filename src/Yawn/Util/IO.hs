module Yawn.Util.IO where

import System.Directory (getDirectoryContents, getModificationTime)
import System.IO.Error (try)
import Yawn.Context (Context, failure)
import Yawn.Util.Time (TimeString, clockTimeToAscDate)
import qualified Data.ByteString as BS (ByteString, readFile)

readDirContentsAndModTime :: Context -> FilePath -> IO (Maybe (TimeString, [FilePath]))
readDirContentsAndModTime ctx path = do
  safeGetModTime ctx path >>= \m -> case m of
    Nothing -> return Nothing
    Just modTime -> tryIO ctx (getDirectoryContents path) >>= \c -> case c of
      Nothing -> return Nothing
      Just contents -> return $ Just (modTime, contents)

readFileAndModTime :: Context -> FilePath -> IO (Maybe (TimeString, BS.ByteString))
readFileAndModTime ctx path = do
  safeGetModTime ctx path >>= \m -> case m of
    Nothing -> return Nothing
    Just modTime -> tryIO ctx (BS.readFile path) >>= \c -> case c of
      Nothing -> return Nothing
      Just content -> return $ Just (modTime, content)

safeGetModTime :: Context -> FilePath -> IO (Maybe (TimeString))
safeGetModTime ctx path = 
  tryIO ctx (clockTimeToAscDate =<< getModificationTime path) 

safeReadFile :: Context -> FilePath -> IO (Maybe (BS.ByteString))
safeReadFile ctx path = 
  tryIO ctx (BS.readFile path) 

tryIO :: Context -> IO (a) -> IO (Maybe a)
tryIO ctx f = try f >>= \o -> case o of
  Left e -> failure ctx (show e) >> return Nothing
  Right ok -> return $ Just ok
