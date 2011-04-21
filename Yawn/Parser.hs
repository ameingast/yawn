module Yawn.Parser where

import Yawn.Data

parseRequest :: String -> Request
parseRequest s = Request GET (ABSOLUTE_PATH "/foo") HTTP_1_1 []
