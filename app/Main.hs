module Main where

import           Bitstamp.Api
import           Network.Wai.Handler.Warp

main :: IO ()
-- main = run 8081 app1
-- main = run 8081 mock1
main = run 8080 bitstampApp

-- -- ws://localhost:8080/bitstamp
-- -- ws://localhost:8080/stream
