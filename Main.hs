module Main(main) where

import Yawn.Data as D
import Yawn.Server as S

main :: IO()
main = do
    let configuration = D.Configuration 9000 "localhost" in S.run configuration 
