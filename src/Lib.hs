{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
module Lib where

import Control.Lens ((&), (.~))
import Control.Lens.TH
import Data.Aeson
import qualified Data.Text as T
import Data.Time.Clock
import Data.Monoid ((<>))
import GHC.Generics
import Network.Wreq

data Cryptocurrency
    = BTC
    | LTC
    | ETH
    | ETC
    | XRP
    | BCH
    deriving (Generic, Show, Eq, Enum, FromJSON, ToJSON)

data Fiat
    = AUD
    | USD
    deriving (Generic, Show, Eq, Enum, FromJSON, ToJSON)

newtype Price = Price Double deriving (Generic, Eq, Ord, FromJSON, ToJSON)

instance Show Price where
    show (Price n) = show n

newtype Amount = Amount Double deriving (Generic, Eq, Ord, FromJSON, ToJSON)

instance Show Amount where
    show (Amount n) = show n

newtype TradeID = TradeID Integer deriving (Generic, Eq, Ord, FromJSON, ToJSON)

instance Show TradeID where
    show (TradeID n) = show n

data Tick = Tick
    { _bestBid    :: Price
    , _bestAsk    :: Price
    , _lastPrice  :: Price
    , _currency   :: Fiat
    , _instrument :: Cryptocurrency
    , _timestamp  :: NominalDiffTime
    , _volume24h  :: Amount
    } deriving (Generic, Show, Eq)

makeFieldsNoPrefix ''Tick

instance FromJSON Tick where
    parseJSON = genericParseJSON defaultOptions {
                    fieldLabelModifier = drop 1 }

instance ToJSON Tick where
    toJSON = genericToJSON defaultOptions {
               fieldLabelModifier = drop 1 }

data Orderbook = Orderbook
    { _currency   :: Fiat
    , _instrument :: Cryptocurrency
    , _timestamp  :: NominalDiffTime
    , _asks       :: [(Price, Amount)]
    , _bids       :: [(Price, Amount)]
    } deriving (Generic, Show, Eq)

makeFieldsNoPrefix ''Orderbook

instance FromJSON Orderbook where
    parseJSON = genericParseJSON defaultOptions {
                    fieldLabelModifier = drop 1 }
    
instance ToJSON Orderbook where
    toJSON = genericToJSON defaultOptions {
               fieldLabelModifier = drop 1 }

data Trade = Trade
    { _tradeID :: TradeID
    , _amount  :: Amount
    , _price   :: Price
    , _date    :: NominalDiffTime
    } deriving (Generic, Show, Eq)

makeFieldsNoPrefix ''Trade

instance FromJSON Trade where
    parseJSON = withObject "trade" $ \o -> Trade
        <$> o .: "tid"
        <*> o .: "amount"
        <*> o .: "price"
        <*> o .: "date"

instance ToJSON Trade where
    toJSON p = object 
      [ "tid"    .= _tradeID p
      , "amount" .= _amount p 
      , "price"  .= _price p
      , "date"   .= _date  p
      ]

baseUrl :: String
baseUrl = "https://api.btcmarkets.net"

tick :: Fiat -> Cryptocurrency -> String
tick fiat crypto =
    baseUrl <> "/market/" <> show crypto <> "/" <> show fiat <> "/tick"

orderbook :: Fiat -> Cryptocurrency -> String
orderbook fiat crypto =
    baseUrl <> "/market/" <> show crypto <> "/" <> show fiat <> "/orderbook"

trades :: Fiat -> Cryptocurrency -> String
trades fiat crypto =
    baseUrl <> "/market/" <> show crypto <> "/" <> show fiat <> "/trades"

getTick :: Fiat -> Cryptocurrency -> IO (Response Tick)
getTick fiat crypto = asJSON =<< get (tick fiat crypto)

getOrderbook :: Fiat -> Cryptocurrency -> IO (Response Orderbook)
getOrderbook fiat crypto = asJSON =<< get (orderbook fiat crypto)

getTrades :: Fiat -> Cryptocurrency -> IO (Response [Trade])
getTrades fiat crypto = asJSON =<< get (trades fiat crypto)

getTradesSince :: Fiat -> Cryptocurrency -> TradeID -> IO (Response [Trade])
getTradesSince fiat crypto tid =
    let opts = defaults & param "since" .~ [T.pack (show tid)]
    in  asJSON =<< getWith opts (trades fiat crypto)
