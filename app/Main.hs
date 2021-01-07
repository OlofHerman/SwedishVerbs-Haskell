module Main (main) where

import Lib (app)
import Network.Wai.Handler.Warp (run)
import RIO (IO)

main :: IO ()
main = run 8080 app
