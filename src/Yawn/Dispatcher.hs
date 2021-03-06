module Yawn.Dispatcher (
  dispatchRequest,
  dispatchError,
  badRequest
) where

import Network.URL (url_path)
import Control.Monad (liftM, liftM2)
import System.Directory (doesFileExist, doesDirectoryExist)
import Yawn.Configuration (Configuration, publicRoot, defaultIndexFile, showIndex, defaultMimeType)
import Yawn.Context (Context, configuration, putResponse, mimeTypes, debug, info, failure)
import Yawn.HTTP.Request
import Yawn.HTTP.Response
import Yawn.Mime (mimeType)
import Yawn.Util.IO (readFileAndModTime, readDirContentsAndModTime)
import Yawn.Util.List (split)
import Yawn.Util.Maybe (fromIOMaybe)
import qualified Data.ByteString.Char8 as BS8 (pack)

dispatchRequest :: Context -> Request -> IO (Maybe (Response))
dispatchRequest ctx r = do
  info ctx $ (show $ method r) ++ " " ++ (requestPath r)
  let safeUrl = not $ elem ".." $ split (== '/') (url_path (url r))
  if not safeUrl 
    then fileNotFound ctx
    else case method r of
      _ ->  dispatchGet ctx r
    
dispatchGet :: Context -> Request -> IO (Maybe (Response))
dispatchGet ctx r = do
  let conf = configuration ctx
  let path = localPath ctx r
  let indexFile = path ++ "/" ++ defaultIndexFile conf
  let requestVersion = Yawn.HTTP.Request.version r
  deliverDefaultIndexFile <- shouldDeliverDefaultIndexFile path indexFile
  deliverDirectoryIndex <- shouldDeliverDirectoryIndex conf path

  if deliverDefaultIndexFile 
    then deliverFile ctx indexFile requestVersion
    else if deliverDirectoryIndex 
      then deliverIndex ctx requestVersion path
      else deliverFile ctx path requestVersion

localPath :: Context -> Request -> FilePath
localPath ctx r = publicRoot (configuration ctx) ++ requestPath r

shouldDeliverDefaultIndexFile :: FilePath -> FilePath -> IO (Bool)
shouldDeliverDefaultIndexFile path indexFile = 
  liftM2 (&&) (doesDirectoryExist path) (doesFileExist indexFile)

shouldDeliverDirectoryIndex :: Configuration -> FilePath -> IO (Bool)
shouldDeliverDirectoryIndex conf path =
  liftM (&& showIndex conf) (doesDirectoryExist path)

deliverIndex :: Context -> HttpVersion -> FilePath -> IO (Maybe (Response))
deliverIndex ctx v path = do
  debug ctx $ "Serving index: " ++ path
  fromIOMaybe (fileNotFound ctx) (readDirContentsAndModTime ctx path) $ \(modTime, list) -> do
    let hs = [CONTENT_TYPE "text/html", LAST_MODIFIED modTime]
    putResponse ctx $ Response OK v hs $ Just $ BS8.pack (show list) 

deliverFile :: Context -> FilePath -> HttpVersion -> IO (Maybe (Response))
deliverFile ctx path v = do
  debug ctx $ "Serving file: " ++ path
  -- check for Header If-Modified-Since
  fromIOMaybe (fileNotFound ctx) (readFileAndModTime ctx path) $ \(modTime, content) -> do
    let hs = [CONTENT_TYPE (contentType ctx path), LAST_MODIFIED modTime]
    putResponse ctx $ Response OK v hs $ Just content

badRequest :: Context -> IO (Maybe (Response))
badRequest ctx = dispatchError ctx BAD_REQUEST HTTP_1_1

fileNotFound :: Context -> IO (Maybe (Response))
fileNotFound ctx = dispatchError ctx NOT_FOUND HTTP_1_1

dispatchError :: Context -> StatusCode -> HttpVersion -> IO (Maybe (Response))
dispatchError ctx sc v = do
  failure ctx $ show sc
  putResponse ctx $ Response sc v [] Nothing

contentType :: Context -> FilePath -> String
contentType ctx path = case mimeType (mimeTypes ctx) path of
  Nothing -> defaultMimeType $ configuration ctx
  Just ct -> ct
