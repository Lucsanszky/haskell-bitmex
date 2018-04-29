{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropMime
import Instances ()

import BitMEX.Model
import BitMEX.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 10) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy APIKey)
      propMimeEq MimeJSON (Proxy :: Proxy AccessToken)
      propMimeEq MimeJSON (Proxy :: Proxy Affiliate)
      propMimeEq MimeJSON (Proxy :: Proxy Announcement)
      propMimeEq MimeJSON (Proxy :: Proxy Chat)
      propMimeEq MimeJSON (Proxy :: Proxy ChatChannels)
      propMimeEq MimeJSON (Proxy :: Proxy ConnectedUsers)
      propMimeEq MimeJSON (Proxy :: Proxy Error)
      propMimeEq MimeJSON (Proxy :: Proxy ErrorError)
      propMimeEq MimeJSON (Proxy :: Proxy Execution)
      propMimeEq MimeJSON (Proxy :: Proxy Funding)
      propMimeEq MimeJSON (Proxy :: Proxy IndexComposite)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse200)
      propMimeEq MimeJSON (Proxy :: Proxy Instrument)
      propMimeEq MimeJSON (Proxy :: Proxy InstrumentInterval)
      propMimeEq MimeJSON (Proxy :: Proxy Insurance)
      propMimeEq MimeJSON (Proxy :: Proxy Leaderboard)
      propMimeEq MimeJSON (Proxy :: Proxy Liquidation)
      propMimeEq MimeJSON (Proxy :: Proxy Margin)
      propMimeEq MimeJSON (Proxy :: Proxy Notification)
      propMimeEq MimeJSON (Proxy :: Proxy Order)
      propMimeEq MimeJSON (Proxy :: Proxy OrderBook)
      propMimeEq MimeJSON (Proxy :: Proxy OrderBookL2)
      propMimeEq MimeJSON (Proxy :: Proxy Position)
      propMimeEq MimeJSON (Proxy :: Proxy Quote)
      propMimeEq MimeJSON (Proxy :: Proxy Settlement)
      propMimeEq MimeJSON (Proxy :: Proxy Stats)
      propMimeEq MimeJSON (Proxy :: Proxy StatsHistory)
      propMimeEq MimeJSON (Proxy :: Proxy StatsUSD)
      propMimeEq MimeJSON (Proxy :: Proxy Trade)
      propMimeEq MimeJSON (Proxy :: Proxy TradeBin)
      propMimeEq MimeJSON (Proxy :: Proxy Transaction)
      propMimeEq MimeJSON (Proxy :: Proxy User)
      propMimeEq MimeJSON (Proxy :: Proxy UserCommission)
      propMimeEq MimeJSON (Proxy :: Proxy UserPreferences)
      propMimeEq MimeJSON (Proxy :: Proxy Wallet)
      propMimeEq MimeJSON (Proxy :: Proxy XAny)
      
