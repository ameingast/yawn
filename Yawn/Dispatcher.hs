module Yawn.Dispatcher (
  dispatchRequest,
  dispatchError,
  badRequest
) where

import Control.Monad (liftM, liftM2)
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.IO.Error (try)
import Yawn.Configuration (Configuration, publicRoot, defaultIndexFile, showIndex, defaultMimeType)
import Yawn.Context (Context, configuration, put, putResponse, mimeTypes)
import Yawn.HTTP.Request
import Yawn.HTTP.Response
import Yawn.Logger (Level (LOG_DEBUG, LOG_INFO, LOG_ERROR), doLog)
import Yawn.Mime (mimeType)
import Yawn.Util.List (split)
import qualified Data.ByteString as BS (readFile)

dispatchRequest :: Context -> Request -> IO (Maybe ())
dispatchRequest ctx r = do
  let conf = configuration ctx
  let path = publicRoot conf ++ requestPath r
  let indexFile = path ++ "/" ++ defaultIndexFile conf
  let safeUrl = not $ elem ".." $ split (== '/') path
  deliverDefaultIndexFile <- shouldDeliverDefaultIndexFile path indexFile
  deliverDirectoryIndex <- shouldDeliverDirectoryIndex conf path

  doLog conf LOG_INFO $ (show $ method r) ++ " " ++ (requestPath r)
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

deliverIndex :: Context -> FilePath -> IO (Maybe ())
deliverIndex ctx path = do
  let conf = configuration ctx
  doLog conf LOG_DEBUG $ "Serving index: " ++ path
  try (getDirectoryContents path) >>= \c -> case c of
    Left e -> (doLog conf LOG_ERROR $ "Unable to index directory " ++ show e) >> internalError ctx
    Right list -> putResponse ctx $ Response OK [CONTENT_TYPE "text/html"] (show list)

deliverFile :: Context -> FilePath -> IO (Maybe ())
deliverFile ctx path = do
  doLog (configuration ctx) LOG_DEBUG $ "Serving file: " ++ path
  try (BS.readFile path) >>= \f -> case f of
    Left _e -> fileNotFound ctx
    Right content -> do 
      putResponse ctx $ Response OK [CONTENT_TYPE (contentType ctx path)] []
      put ctx content

badRequest :: Context -> IO (Maybe ())
badRequest ctx = dispatchError ctx BAD_REQUEST

internalError :: Context -> IO (Maybe ())
internalError ctx = dispatchError ctx INTERNAL_ERROR

fileNotFound :: Context -> IO (Maybe ())
fileNotFound ctx = dispatchError ctx NOT_FOUND

dispatchError :: Context -> StatusCode -> IO (Maybe ())
dispatchError ctx sc = do
  doLog (configuration ctx) LOG_ERROR sc
  putResponse ctx $ Response sc [] ""

contentType :: Context -> FilePath -> String
contentType ctx path = case mimeType (mimeTypes ctx) path of
  Nothing -> defaultMimeType $ configuration ctx
  Just ct -> ct
