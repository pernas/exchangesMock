{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Bitstamp.Api
  ( bitstampApp
  ) where

import           Bitstamp.Types
import           Control.Monad
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Concurrent
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Conduit                 
import qualified Data.Conduit.List            as CL
import           Data.Foldable                (forM_)
import           Data.Text                    (Text, pack)
import           Network.WebSockets           (Connection, forkPingThread,
                                               sendTextData)
import           Servant
import           Servant.API.WebSocket
import           Network.WebSockets.Connection
import           Servant.API.WebSocketConduit (WebSocketConduit)
-- import           Servant.Mock

-- routes
type TradingPairs
  = "bitstamp"
  :> "api"
  :> "v2"
  :> "trading-pairs-info"
  :> Get '[ JSON] [BitstampPair]

type WebSocketStream =
  "bitstamp"
  :> WebSocket

-- API
type BitstampApi = TradingPairs :<|> WebSocketStream

bitstampAPI :: Proxy BitstampApi
bitstampAPI = Proxy

bitstampApp :: Application
bitstampApp = serve bitstampAPI server

-- app "handlers"
server :: Server BitstampApi
server = discoveryHandler :<|> streamData
  where
    streamData :: Connection -> Handler ()
    streamData c =
      liftIO . forever $ do
        msg <- eitherDecode <$> receiveData c :: IO (Either String (Message Channel))
        case msg of
          Right m -> handleMessage c m 
          Left e -> sendTextData c $ encode subscriptionFailed
      
    discoveryHandler :: Handler [BitstampPair]
    discoveryHandler = return example

-- websocket, handle subscription
handleMessage :: Connection -> Message Channel -> IO ()
handleMessage c m = do
  let product = channel_channel $ message_data m
  -- handle logic for different products
  sendTextData c $ encode $ subscriptionSucceeded product
  subscribe c product

subscribe :: Connection -> Text -> IO ()  
subscribe c p = void $ forkIO $ forM_ [1 ..] $ \_ -> do
  forkPingThread c 10 -- keep alive
  sendTextData c (encode $ messageTrade p) >> threadDelay 1000000 -- send same trade every second

-- mocked server // websocket does not provide HasMock typeclass implementation
-- mockBitstampApp :: Application
-- mockBitstampApp = serve bitstampAPI (mock bitstampAPI Proxy)
