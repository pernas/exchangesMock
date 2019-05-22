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
import           Control.Concurrent           (threadDelay)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Data.Aeson.Types
import           Data.Conduit                 (ConduitT)
import qualified Data.Conduit.List            as CL
import           Data.Foldable                (forM_)
import           Data.Text                    (Text, pack)
import           Network.WebSockets           (Connection, forkPingThread,
                                               sendTextData)
import           Servant
import           Servant.API.WebSocket        (WebSocket)
import           Servant.API.WebSocketConduit (WebSocketConduit)

-- import           Servant.Mock
-- routes
type TradingPairs
   = "bitstamp" :> "api" :> "v2" :> "trading-pairs-info" :> Get '[ JSON] [BitstampPair]

type WebSocketTrades = "bitstamp" :> WebSocketConduit Value Value

type WebSocketApi = "stream" :> WebSocket

-- API
type BitstampApi = TradingPairs :<|> WebSocketTrades :<|> WebSocketApi

-- handlers
type Conduit i m o = ConduitT i o m ()

echo :: Monad m => Conduit Value m Value
echo = CL.map id

-- app "handlers"
server :: Server BitstampApi
server = return example :<|> echo :<|> streamData
  where
    streamData :: MonadIO m => Connection -> m ()
    streamData c =
      liftIO . forM_ [1 ..] $ \i -> do
        forkPingThread c 10
        sendTextData c (pack $ show (i :: Int)) >> threadDelay 1000000

bitstampAPI :: Proxy BitstampApi
bitstampAPI = Proxy

bitstampApp :: Application
bitstampApp = serve bitstampAPI server

-- mocked server // websocket does not provide HasMock typeclass implementation
-- mockBitstampApp :: Application
-- mockBitstampApp = serve bitstampAPI (mock bitstampAPI Proxy)
