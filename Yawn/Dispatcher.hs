module Yawn.Dispatcher (
  dispatchRequest,
  dispatchError,
  badRequest
) where

import Control.Monad (liftM, liftM2)
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.IO.Error (try)
import Yawn.Configuration (Configuration, publicRoot, defaultIndexFile, showIndex)
import Yawn.Context (Context, configuration, put, putBin, mimeTypes)
import Yawn.HTTP.Request
import Yawn.HTTP.Response
import Yawn.Logger (Level (LOG_DEBUG, LOG_ERROR), doLog)
import Yawn.Mime (mimeType)
import Yawn.Util.List (split)
import qualified Data.ByteString as BS (readFile)

dispatchRequest :: Context -> Request -> IO ()
dispatchRequest ctx r = do
  let conf = configuration ctx
  let path = publicRoot conf ++ requestPath r
  let indexFile = path ++ "/" ++ defaultIndexFile conf
  let safeUrl = not $ elem ".." $ split (== '/') path
  deliverDefaultIndexFile <- shouldDeliverDefaultIndexFile conf path indexFile
  deliverDirectoryIndex <- shouldDeliverDirectoryIndex conf path
  if not safeUrl then fileNotFound ctx
  else if deliverDefaultIndexFile then deliverFile ctx indexFile
    else if deliverDirectoryIndex then deliverIndex ctx path
         else deliverFile ctx path

shouldDeliverDefaultIndexFile :: Configuration -> FilePath -> FilePath -> IO (Bool)
shouldDeliverDefaultIndexFile conf path indexFile = 
  liftM2 (&&) (doesDirectoryExist path) (doesFileExist indexFile)

shouldDeliverDirectoryIndex :: Configuration -> FilePath -> IO (Bool)
shouldDeliverDirectoryIndex conf path =
  liftM (&& showIndex conf) (doesDirectoryExist path)

deliverIndex :: Context -> FilePath -> IO ()
deliverIndex ctx path = do
  let conf = configuration ctx
  doLog conf LOG_DEBUG $ "Serving index: " ++ path
  try (getDirectoryContents path) >>= \c -> case c of
    Left e -> (doLog conf LOG_ERROR $ "Unable to index directory " ++ show e) >> internalError ctx
    Right list -> put ctx $ show $ Response OK [CONTENT_TYPE "text/html"] (show list)

deliverFile :: Context -> FilePath -> IO ()
deliverFile ctx path = do
  doLog (configuration ctx) LOG_DEBUG $ "Serving file: " ++ path
  try (BS.readFile path) >>= \f -> case f of
    Left _e -> fileNotFound ctx
    Right content -> do 
      put ctx $ show $ Response OK [CONTENT_TYPE (contentType ctx path)] []
      putBin ctx content

badRequest :: Context -> IO ()
badRequest ctx = dispatchError ctx BAD_REQUEST

internalError :: Context -> IO ()
internalError ctx = dispatchError ctx INTERNAL_ERROR

fileNotFound :: Context -> IO ()
fileNotFound ctx = dispatchError ctx NOT_FOUND

dispatchError :: Context -> StatusCode -> IO ()
dispatchError ctx sc = do
  doLog (configuration ctx) LOG_ERROR sc
  put ctx $ show $ Response sc [] ""

contentType :: Context -> FilePath -> String
contentType ctx path = case mimeType (mimeTypes ctx) path of
  Nothing -> ""
  Just ct -> ct
