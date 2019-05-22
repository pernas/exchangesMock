{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Bitstamp.Types
  ( BitstampPair (..)
  , example
  ) where

import           Data.Aeson.Types
import           Data.Text                    (Text)
import           GHC.Generics
import           Test.QuickCheck
import           Test.QuickCheck.Instances

data BitstampPair = BitstampPair
  { base_decimals    :: Int
  , minimum_order    :: Text
  , name             :: Text
  , counter_decimals :: Int
  , trading          :: Text
  , url_symbol       :: Text
  , description      :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

example :: [BitstampPair]
example = 
  [ BitstampPair 8 "5.0 USD" "LTC/USD" 2 "Enabled" "ltcusd" "Litecoin / U.S. dollar"
  , BitstampPair 8 "5.0 USD" "ETH/USD" 2 "Enabled" "ethusd" "Ether / U.S. dollar"
  ]

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