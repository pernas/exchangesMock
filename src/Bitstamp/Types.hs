{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE LambdaCase                 #-}

module Bitstamp.Types
  ( BitstampPair (..)
  , Message (..)
  , Channel (..)
  , Trade (..)
  , example
  , messageTrade
  , subscriptionSucceeded
  , subscriptionFailed
  ) where

import           Data.Aeson.Types
import           Data.Text                    (Text)
import           GHC.Generics
import           Test.QuickCheck
import           Test.QuickCheck.Instances

jsonOptions :: Options
jsonOptions = defaultOptions
  { fieldLabelModifier = dropBeforeUnderscore
  , constructorTagModifier = dropBeforeUnderscore
  } where
  dropBeforeUnderscore = \case
    x : xs -> case x of
      '_' -> xs
      _ -> dropBeforeUnderscore xs
    [] -> []

data BitstampPair = BitstampPair
  { pair_base_decimals    :: Int
  , pair_minimum_order    :: Text
  , pair_name             :: Text
  , pair_counter_decimals :: Int
  , pair_trading          :: Text
  , pair_url_symbol       :: Text
  , pair_description      :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON BitstampPair where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON BitstampPair where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

example :: [BitstampPair]
example = 
  [ BitstampPair 8 "5.0 USD" "LTC/USD" 2 "Enabled" "ltcusd" "Litecoin / U.S. dollar"
  , BitstampPair 8 "5.0 USD" "ETH/USD" 2 "Enabled" "ethusd" "Ether / U.S. dollar"
  ]

data Channel = Channel { channel_channel :: Text } deriving (Eq, Show, Generic)

instance FromJSON Channel where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Channel where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

data Message a = Message
  { message_event   :: Text
  , message_data    :: a
  , message_channel :: Maybe Channel
  } deriving (Eq, Show, Generic)

instance FromJSON a => FromJSON (Message a) where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON a => ToJSON (Message a) where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

subscriptionSucceeded :: Text -> Message Channel
subscriptionSucceeded p = Message "bts:subscription_succeeded" (Channel p) Nothing

-- fix that type,this is not channel
subscriptionFailed :: Message Channel
subscriptionFailed = Message "bts:error" (Channel "Bad subscription string.") Nothing

-- fix types (no double, maybe not Integer for id)
data Trade = Trade
  { trade_trade_id       :: Integer
  , trade_amount         :: Double
  , trade_amount_str     :: Text
  , trade_price          :: Double
  , trade_price_str      :: Text
  , trade_timestamp      :: Text
  , trade_microtimestamp :: Text
  , trade_buy_order_id   :: Integer
  , trade_sell_order_id  :: Integer
  , trade_type           :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON Trade where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Trade where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

-- this is a real response from Bitstamp, WTF, 14999 =/= 15 :D
exampleTrade = Trade 88774747 0.02 "0.02000000" 7691.1499999999996 "7691.15"
 "1558564279" "1558564279547537" 3350315317 3350316782 1

messageTrade :: Text -> Message Trade
messageTrade p = (Message "trade") exampleTrade (Just $ Channel p)

-- -- necessary for mocked server // TODO: control randomness of data
-- instance Arbitrary BitstampPair where
--   arbitrary = BitstampPair
--     <$> arbitrary
--     <*> arbitrary
--     <*> arbitrary
--     <*> arbitrary
--     <*> arbitrary
--     <*> arbitrary
--     <*> arbitrary