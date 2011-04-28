module Yawn.Dispatcher (
  dispatchRequest,
  dispatchError,
  badRequest
) where

import Control.Monad (liftM, liftM2)
import System.Directory (doesFileExist, doesDirectoryExist)
import Yawn.Configuration (Configuration, publicRoot, defaultIndexFile, showIndex, defaultMimeType)
import Yawn.Context (Context, configuration, putResponse, mimeTypes, debug, info, failure)
import Yawn.HTTP.Request
import Yawn.HTTP.Response
import Yawn.Mime (mimeType)
import Yawn.Util.List (split)
import Yawn.Util.IO (readFileAndModTime, readDirContentsAndModTime)
import qualified Data.ByteString.Char8 as BS8 (pack)

dispatchRequest :: Context -> Request -> IO (Maybe ())
dispatchRequest ctx r = do
  let conf = configuration ctx
  let path = publicRoot conf ++ requestPath r
  let indexFile = path ++ "/" ++ defaultIndexFile conf
  let safeUrl = not $ elem ".." $ split (== '/') path
  deliverDefaultIndexFile <- shouldDeliverDefaultIndexFile path indexFile
  deliverDirectoryIndex <- shouldDeliverDirectoryIndex conf path

  info ctx $ (show $ method r) ++ " " ++ (requestPath r)
  if not safeUrl then fileNotFound ctx
  else if deliverDefaultIndexFile then deliverFile ctx indexFile
    else if deliverDirectoryIndex then deliverIndex ctx path
         else deliverFile ctx path

shouldDeliverDefaultIndexFile :: FilePath -> FilePath -> IO (Bool)
shouldDeliverDefaultIndexFile path indexFile = 
  liftM2 (&&) (doesDirectoryExist path) (doesFileExist indexFile)

shouldDeliverDirectoryIndex :: Configuration -> FilePath -> IO (Bool)
shouldDeliverDirectoryIndex conf path =
  liftM (&& showIndex conf) (doesDirectoryExist path)

-- replace with template
deliverIndex :: Context -> FilePath -> IO (Maybe ())
deliverIndex ctx path = do
  debug ctx $ "Serving index: " ++ path
  readDirContentsAndModTime ctx path >>= \c -> case c of
    Nothing -> fileNotFound ctx
    Just (modTime, list) -> do
      let hs = [CONTENT_TYPE "text/html", LAST_MODIFIED modTime]
      putResponse ctx $ Response OK hs $ Just $ BS8.pack (show list) 

deliverFile :: Context -> FilePath -> IO (Maybe ())
deliverFile ctx path = do
  debug ctx $ "Serving file: " ++ path
  readFileAndModTime ctx path >>= \file -> case file of
    Nothing -> fileNotFound ctx
    Just (modTime, content) -> do
      let hs = [CONTENT_TYPE (contentType ctx path), LAST_MODIFIED modTime]
      putResponse ctx $ Response OK hs $ Just content

badRequest :: Context -> IO (Maybe ())
badRequest ctx = dispatchError ctx BAD_REQUEST

fileNotFound :: Context -> IO (Maybe ())
fileNotFound ctx = dispatchError ctx NOT_FOUND

dispatchError :: Context -> StatusCode -> IO (Maybe ())
dispatchError ctx sc = do
  failure ctx $ show sc
  putResponse ctx $ Response sc [] Nothing

contentType :: Context -> FilePath -> String
contentType ctx path = case mimeType (mimeTypes ctx) path of
  Nothing -> defaultMimeType $ configuration ctx
  Just ct -> ct
