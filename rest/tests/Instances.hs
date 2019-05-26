{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Instances where

import BitMEX.Model
import BitMEX.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (T.pack k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays

-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

-- * Models

instance Arbitrary APIKey where
  arbitrary =
    APIKey
      <$> arbitrary -- aPIKeyId :: Text
      <*> arbitrary -- aPIKeySecret :: Text
      <*> arbitrary -- aPIKeyName :: Text
      <*> arbitrary -- aPIKeyNonce :: Double
      <*> arbitrary -- aPIKeyCidr :: Maybe Text
      <*> arbitrary -- aPIKeyPermissions :: Maybe [XAny]
      <*> arbitrary -- aPIKeyEnabled :: Maybe Bool
      <*> arbitrary -- aPIKeyUserId :: Double
      <*> arbitrary -- aPIKeyCreated :: Maybe DateTime

instance Arbitrary AccessToken where
  arbitrary =
    AccessToken
      <$> arbitrary -- accessTokenId :: Text
      <*> arbitrary -- accessTokenTtl :: Maybe Double
      <*> arbitrary -- accessTokenCreated :: Maybe DateTime
      <*> arbitrary -- accessTokenUserId :: Maybe Double

instance Arbitrary Affiliate where
  arbitrary =
    Affiliate
      <$> arbitrary -- affiliateAccount :: Double
      <*> arbitrary -- affiliateCurrency :: Text
      <*> arbitrary -- affiliatePrevPayout :: Maybe Double
      <*> arbitrary -- affiliatePrevTurnover :: Maybe Double
      <*> arbitrary -- affiliatePrevComm :: Maybe Double
      <*> arbitrary -- affiliatePrevTimestamp :: Maybe DateTime
      <*> arbitrary -- affiliateExecTurnover :: Maybe Double
      <*> arbitrary -- affiliateExecComm :: Maybe Double
      <*> arbitrary -- affiliateTotalReferrals :: Maybe Double
      <*> arbitrary -- affiliateTotalTurnover :: Maybe Double
      <*> arbitrary -- affiliateTotalComm :: Maybe Double
      <*> arbitrary -- affiliatePayoutPcnt :: Maybe Double
      <*> arbitrary -- affiliatePendingPayout :: Maybe Double
      <*> arbitrary -- affiliateTimestamp :: Maybe DateTime
      <*> arbitrary -- affiliateReferrerAccount :: Maybe Double

instance Arbitrary Announcement where
  arbitrary =
    Announcement
      <$> arbitrary -- announcementId :: Double
      <*> arbitrary -- announcementLink :: Maybe Text
      <*> arbitrary -- announcementTitle :: Maybe Text
      <*> arbitrary -- announcementContent :: Maybe Text
      <*> arbitrary -- announcementDate :: Maybe DateTime

instance Arbitrary Chat where
  arbitrary =
    Chat
      <$> arbitrary -- chatId :: Maybe Double
      <*> arbitrary -- chatDate :: DateTime
      <*> arbitrary -- chatUser :: Text
      <*> arbitrary -- chatMessage :: Text
      <*> arbitrary -- chatHtml :: Text
      <*> arbitrary -- chatFromBot :: Maybe Bool
      <*> arbitrary -- chatChannelId :: Maybe Double

instance Arbitrary ChatChannels where
  arbitrary =
    ChatChannels
      <$> arbitrary -- chatChannelsId :: Maybe Double
      <*> arbitrary -- chatChannelsName :: Text

instance Arbitrary ConnectedUsers where
  arbitrary =
    ConnectedUsers
      <$> arbitrary -- connectedUsersUsers :: Maybe Double
      <*> arbitrary -- connectedUsersBots :: Maybe Double

instance Arbitrary Error where
  arbitrary =
    Error
      <$> arbitrary -- errorError :: ErrorError

instance Arbitrary ErrorError where
  arbitrary =
    ErrorError
      <$> arbitrary -- errorErrorMessage :: Maybe Text
      <*> arbitrary -- errorErrorName :: Maybe Text

instance Arbitrary Execution where
  arbitrary =
    Execution
      <$> arbitrary -- executionExecId :: Text
      <*> arbitrary -- executionOrderId :: Maybe Text
      <*> arbitrary -- executionClOrdId :: Maybe Text
      <*> arbitrary -- executionClOrdLinkId :: Maybe Text
      <*> arbitrary -- executionAccount :: Maybe Double
      <*> arbitrary -- executionSymbol :: Maybe Text
      <*> arbitrary -- executionSide :: Maybe Text
      <*> arbitrary -- executionLastQty :: Maybe Double
      <*> arbitrary -- executionLastPx :: Maybe Double
      <*> arbitrary -- executionUnderlyingLastPx :: Maybe Double
      <*> arbitrary -- executionLastMkt :: Maybe Text
      <*> arbitrary -- executionLastLiquidityInd :: Maybe Text
      <*> arbitrary -- executionSimpleOrderQty :: Maybe Double
      <*> arbitrary -- executionOrderQty :: Maybe Double
      <*> arbitrary -- executionPrice :: Maybe Double
      <*> arbitrary -- executionDisplayQty :: Maybe Double
      <*> arbitrary -- executionStopPx :: Maybe Double
      <*> arbitrary -- executionPegOffsetValue :: Maybe Double
      <*> arbitrary -- executionPegPriceType :: Maybe Text
      <*> arbitrary -- executionCurrency :: Maybe Text
      <*> arbitrary -- executionSettlCurrency :: Maybe Text
      <*> arbitrary -- executionExecType :: Maybe Text
      <*> arbitrary -- executionOrdType :: Maybe Text
      <*> arbitrary -- executionTimeInForce :: Maybe Text
      <*> arbitrary -- executionExecInst :: Maybe Text
      <*> arbitrary -- executionContingencyType :: Maybe Text
      <*> arbitrary -- executionExDestination :: Maybe Text
      <*> arbitrary -- executionOrdStatus :: Maybe Text
      <*> arbitrary -- executionTriggered :: Maybe Text
      <*> arbitrary -- executionWorkingIndicator :: Maybe Bool
      <*> arbitrary -- executionOrdRejReason :: Maybe Text
      <*> arbitrary -- executionSimpleLeavesQty :: Maybe Double
      <*> arbitrary -- executionLeavesQty :: Maybe Double
      <*> arbitrary -- executionSimpleCumQty :: Maybe Double
      <*> arbitrary -- executionCumQty :: Maybe Double
      <*> arbitrary -- executionAvgPx :: Maybe Double
      <*> arbitrary -- executionCommission :: Maybe Double
      <*> arbitrary -- executionTradePublishIndicator :: Maybe Text
      <*> arbitrary -- executionMultiLegReportingType :: Maybe Text
      <*> arbitrary -- executionText :: Maybe Text
      <*> arbitrary -- executionTrdMatchId :: Maybe Text
      <*> arbitrary -- executionExecCost :: Maybe Double
      <*> arbitrary -- executionExecComm :: Maybe Double
      <*> arbitrary -- executionHomeNotional :: Maybe Double
      <*> arbitrary -- executionForeignNotional :: Maybe Double
      <*> arbitrary -- executionTransactTime :: Maybe DateTime
      <*> arbitrary -- executionTimestamp :: Maybe DateTime

instance Arbitrary Funding where
  arbitrary =
    Funding
      <$> arbitrary -- fundingTimestamp :: DateTime
      <*> arbitrary -- fundingSymbol :: Text
      <*> arbitrary -- fundingFundingInterval :: Maybe DateTime
      <*> arbitrary -- fundingFundingRate :: Maybe Double
      <*> arbitrary -- fundingFundingRateDaily :: Maybe Double

instance Arbitrary IndexComposite where
  arbitrary =
    IndexComposite
      <$> arbitrary -- indexCompositeTimestamp :: DateTime
      <*> arbitrary -- indexCompositeSymbol :: Maybe Text
      <*> arbitrary -- indexCompositeIndexSymbol :: Maybe Text
      <*> arbitrary -- indexCompositeReference :: Maybe Text
      <*> arbitrary -- indexCompositeLastPrice :: Maybe Double
      <*> arbitrary -- indexCompositeWeight :: Maybe Double
      <*> arbitrary -- indexCompositeLogged :: Maybe DateTime

instance Arbitrary InlineResponse200 where
  arbitrary =
    InlineResponse200
      <$> arbitrary -- inlineResponse200Success :: Maybe Bool

instance Arbitrary Instrument where
  arbitrary =
    Instrument
      <$> arbitrary -- instrumentSymbol :: Text
      <*> arbitrary -- instrumentRootSymbol :: Maybe Text
      <*> arbitrary -- instrumentState :: Maybe Text
      <*> arbitrary -- instrumentTyp :: Maybe Text
      <*> arbitrary -- instrumentListing :: Maybe DateTime
      <*> arbitrary -- instrumentFront :: Maybe DateTime
      <*> arbitrary -- instrumentExpiry :: Maybe DateTime
      <*> arbitrary -- instrumentSettle :: Maybe DateTime
      <*> arbitrary -- instrumentRelistInterval :: Maybe DateTime
      <*> arbitrary -- instrumentInverseLeg :: Maybe Text
      <*> arbitrary -- instrumentSellLeg :: Maybe Text
      <*> arbitrary -- instrumentBuyLeg :: Maybe Text
      <*> arbitrary -- instrumentPositionCurrency :: Maybe Text
      <*> arbitrary -- instrumentUnderlying :: Maybe Text
      <*> arbitrary -- instrumentQuoteCurrency :: Maybe Text
      <*> arbitrary -- instrumentUnderlyingSymbol :: Maybe Text
      <*> arbitrary -- instrumentReference :: Maybe Text
      <*> arbitrary -- instrumentReferenceSymbol :: Maybe Text
      <*> arbitrary -- instrumentCalcInterval :: Maybe DateTime
      <*> arbitrary -- instrumentPublishInterval :: Maybe DateTime
      <*> arbitrary -- instrumentPublishTime :: Maybe DateTime
      <*> arbitrary -- instrumentMaxOrderQty :: Maybe Double
      <*> arbitrary -- instrumentMaxPrice :: Maybe Double
      <*> arbitrary -- instrumentLotSize :: Maybe Double
      <*> arbitrary -- instrumentTickSize :: Maybe Double
      <*> arbitrary -- instrumentMultiplier :: Maybe Double
      <*> arbitrary -- instrumentSettlCurrency :: Maybe Text
      <*> arbitrary -- instrumentUnderlyingToPositionMultiplier :: Maybe Double
      <*> arbitrary -- instrumentUnderlyingToSettleMultiplier :: Maybe Double
      <*> arbitrary -- instrumentQuoteToSettleMultiplier :: Maybe Double
      <*> arbitrary -- instrumentIsQuanto :: Maybe Bool
      <*> arbitrary -- instrumentIsInverse :: Maybe Bool
      <*> arbitrary -- instrumentInitMargin :: Maybe Double
      <*> arbitrary -- instrumentMaintMargin :: Maybe Double
      <*> arbitrary -- instrumentRiskLimit :: Maybe Double
      <*> arbitrary -- instrumentRiskStep :: Maybe Double
      <*> arbitrary -- instrumentLimit :: Maybe Double
      <*> arbitrary -- instrumentCapped :: Maybe Bool
      <*> arbitrary -- instrumentTaxed :: Maybe Bool
      <*> arbitrary -- instrumentDeleverage :: Maybe Bool
      <*> arbitrary -- instrumentMakerFee :: Maybe Double
      <*> arbitrary -- instrumentTakerFee :: Maybe Double
      <*> arbitrary -- instrumentSettlementFee :: Maybe Double
      <*> arbitrary -- instrumentInsuranceFee :: Maybe Double
      <*> arbitrary -- instrumentFundingBaseSymbol :: Maybe Text
      <*> arbitrary -- instrumentFundingQuoteSymbol :: Maybe Text
      <*> arbitrary -- instrumentFundingPremiumSymbol :: Maybe Text
      <*> arbitrary -- instrumentFundingTimestamp :: Maybe DateTime
      <*> arbitrary -- instrumentFundingInterval :: Maybe DateTime
      <*> arbitrary -- instrumentFundingRate :: Maybe Double
      <*> arbitrary -- instrumentIndicativeFundingRate :: Maybe Double
      <*> arbitrary -- instrumentRebalanceTimestamp :: Maybe DateTime
      <*> arbitrary -- instrumentRebalanceInterval :: Maybe DateTime
      <*> arbitrary -- instrumentOpeningTimestamp :: Maybe DateTime
      <*> arbitrary -- instrumentClosingTimestamp :: Maybe DateTime
      <*> arbitrary -- instrumentSessionInterval :: Maybe DateTime
      <*> arbitrary -- instrumentPrevClosePrice :: Maybe Double
      <*> arbitrary -- instrumentLimitDownPrice :: Maybe Double
      <*> arbitrary -- instrumentLimitUpPrice :: Maybe Double
      <*> arbitrary -- instrumentBankruptLimitDownPrice :: Maybe Double
      <*> arbitrary -- instrumentBankruptLimitUpPrice :: Maybe Double
      <*> arbitrary -- instrumentPrevTotalVolume :: Maybe Double
      <*> arbitrary -- instrumentTotalVolume :: Maybe Double
      <*> arbitrary -- instrumentVolume :: Maybe Double
      <*> arbitrary -- instrumentVolume24h :: Maybe Double
      <*> arbitrary -- instrumentPrevTotalTurnover :: Maybe Double
      <*> arbitrary -- instrumentTotalTurnover :: Maybe Double
      <*> arbitrary -- instrumentTurnover :: Maybe Double
      <*> arbitrary -- instrumentTurnover24h :: Maybe Double
      <*> arbitrary -- instrumentPrevPrice24h :: Maybe Double
      <*> arbitrary -- instrumentVwap :: Maybe Double
      <*> arbitrary -- instrumentHighPrice :: Maybe Double
      <*> arbitrary -- instrumentLowPrice :: Maybe Double
      <*> arbitrary -- instrumentLastPrice :: Maybe Double
      <*> arbitrary -- instrumentLastPriceProtected :: Maybe Double
      <*> arbitrary -- instrumentLastTickDirection :: Maybe Text
      <*> arbitrary -- instrumentLastChangePcnt :: Maybe Double
      <*> arbitrary -- instrumentBidPrice :: Maybe Double
      <*> arbitrary -- instrumentMidPrice :: Maybe Double
      <*> arbitrary -- instrumentAskPrice :: Maybe Double
      <*> arbitrary -- instrumentImpactBidPrice :: Maybe Double
      <*> arbitrary -- instrumentImpactMidPrice :: Maybe Double
      <*> arbitrary -- instrumentImpactAskPrice :: Maybe Double
      <*> arbitrary -- instrumentHasLiquidity :: Maybe Bool
      <*> arbitrary -- instrumentOpenInterest :: Maybe Double
      <*> arbitrary -- instrumentOpenValue :: Maybe Double
      <*> arbitrary -- instrumentFairMethod :: Maybe Text
      <*> arbitrary -- instrumentFairBasisRate :: Maybe Double
      <*> arbitrary -- instrumentFairBasis :: Maybe Double
      <*> arbitrary -- instrumentFairPrice :: Maybe Double
      <*> arbitrary -- instrumentMarkMethod :: Maybe Text
      <*> arbitrary -- instrumentMarkPrice :: Maybe Double
      <*> arbitrary -- instrumentIndicativeTaxRate :: Maybe Double
      <*> arbitrary -- instrumentIndicativeSettlePrice :: Maybe Double
      <*> arbitrary -- instrumentSettledPrice :: Maybe Double
      <*> arbitrary -- instrumentTimestamp :: Maybe DateTime

instance Arbitrary InstrumentInterval where
  arbitrary =
    InstrumentInterval
      <$> arbitrary -- instrumentIntervalIntervals :: [Text]
      <*> arbitrary -- instrumentIntervalSymbols :: [Text]

instance Arbitrary Insurance where
  arbitrary =
    Insurance
      <$> arbitrary -- insuranceCurrency :: Text
      <*> arbitrary -- insuranceTimestamp :: DateTime
      <*> arbitrary -- insuranceWalletBalance :: Maybe Double

instance Arbitrary Leaderboard where
  arbitrary =
    Leaderboard
      <$> arbitrary -- leaderboardName :: Text
      <*> arbitrary -- leaderboardIsRealName :: Maybe Bool
      <*> arbitrary -- leaderboardIsMe :: Maybe Bool
      <*> arbitrary -- leaderboardProfit :: Maybe Double

instance Arbitrary Liquidation where
  arbitrary =
    Liquidation
      <$> arbitrary -- liquidationOrderId :: Text
      <*> arbitrary -- liquidationSymbol :: Maybe Text
      <*> arbitrary -- liquidationSide :: Maybe Text
      <*> arbitrary -- liquidationPrice :: Maybe Double
      <*> arbitrary -- liquidationLeavesQty :: Maybe Double

instance Arbitrary Margin where
  arbitrary =
    Margin
      <$> arbitrary -- marginAccount :: Double
      <*> arbitrary -- marginCurrency :: Text
      <*> arbitrary -- marginRiskLimit :: Maybe Double
      <*> arbitrary -- marginPrevState :: Maybe Text
      <*> arbitrary -- marginState :: Maybe Text
      <*> arbitrary -- marginAction :: Maybe Text
      <*> arbitrary -- marginAmount :: Maybe Double
      <*> arbitrary -- marginPendingCredit :: Maybe Double
      <*> arbitrary -- marginPendingDebit :: Maybe Double
      <*> arbitrary -- marginConfirmedDebit :: Maybe Double
      <*> arbitrary -- marginPrevRealisedPnl :: Maybe Double
      <*> arbitrary -- marginPrevUnrealisedPnl :: Maybe Double
      <*> arbitrary -- marginGrossComm :: Maybe Double
      <*> arbitrary -- marginGrossOpenCost :: Maybe Double
      <*> arbitrary -- marginGrossOpenPremium :: Maybe Double
      <*> arbitrary -- marginGrossExecCost :: Maybe Double
      <*> arbitrary -- marginGrossMarkValue :: Maybe Double
      <*> arbitrary -- marginRiskValue :: Maybe Double
      <*> arbitrary -- marginTaxableMargin :: Maybe Double
      <*> arbitrary -- marginInitMargin :: Maybe Double
      <*> arbitrary -- marginMaintMargin :: Maybe Double
      <*> arbitrary -- marginSessionMargin :: Maybe Double
      <*> arbitrary -- marginTargetExcessMargin :: Maybe Double
      <*> arbitrary -- marginVarMargin :: Maybe Double
      <*> arbitrary -- marginRealisedPnl :: Maybe Double
      <*> arbitrary -- marginUnrealisedPnl :: Maybe Double
      <*> arbitrary -- marginIndicativeTax :: Maybe Double
      <*> arbitrary -- marginUnrealisedProfit :: Maybe Double
      <*> arbitrary -- marginSyntheticMargin :: Maybe Double
      <*> arbitrary -- marginWalletBalance :: Maybe Double
      <*> arbitrary -- marginMarginBalance :: Maybe Double
      <*> arbitrary -- marginMarginBalancePcnt :: Maybe Double
      <*> arbitrary -- marginMarginLeverage :: Maybe Double
      <*> arbitrary -- marginMarginUsedPcnt :: Maybe Double
      <*> arbitrary -- marginExcessMargin :: Maybe Double
      <*> arbitrary -- marginExcessMarginPcnt :: Maybe Double
      <*> arbitrary -- marginAvailableMargin :: Maybe Double
      <*> arbitrary -- marginWithdrawableMargin :: Maybe Double
      <*> arbitrary -- marginTimestamp :: Maybe DateTime
      <*> arbitrary -- marginGrossLastValue :: Maybe Double
      <*> arbitrary -- marginCommission :: Maybe Double

instance Arbitrary Notification where
  arbitrary =
    Notification
      <$> arbitrary -- notificationId :: Maybe Double
      <*> arbitrary -- notificationDate :: DateTime
      <*> arbitrary -- notificationTitle :: Text
      <*> arbitrary -- notificationBody :: Text
      <*> arbitrary -- notificationTtl :: Double
      <*> arbitrary -- notificationType :: Maybe Text
      <*> arbitrary -- notificationClosable :: Maybe Bool
      <*> arbitrary -- notificationPersist :: Maybe Bool
      <*> arbitrary -- notificationWaitForVisibility :: Maybe Bool
      <*> arbitrary -- notificationSound :: Maybe Text

instance Arbitrary Order where
  arbitrary =
    Order
      <$> arbitrary -- orderOrderId :: Text
      <*> arbitrary -- orderClOrdId :: Maybe Text
      <*> arbitrary -- orderClOrdLinkId :: Maybe Text
      <*> arbitrary -- orderAccount :: Maybe Double
      <*> arbitrary -- orderSymbol :: Maybe Text
      <*> arbitrary -- orderSide :: Maybe Text
      <*> arbitrary -- orderSimpleOrderQty :: Maybe Double
      <*> arbitrary -- orderOrderQty :: Maybe Double
      <*> arbitrary -- orderPrice :: Maybe Double
      <*> arbitrary -- orderDisplayQty :: Maybe Double
      <*> arbitrary -- orderStopPx :: Maybe Double
      <*> arbitrary -- orderPegOffsetValue :: Maybe Double
      <*> arbitrary -- orderPegPriceType :: Maybe Text
      <*> arbitrary -- orderCurrency :: Maybe Text
      <*> arbitrary -- orderSettlCurrency :: Maybe Text
      <*> arbitrary -- orderOrdType :: Maybe Text
      <*> arbitrary -- orderTimeInForce :: Maybe Text
      <*> arbitrary -- orderExecInst :: Maybe Text
      <*> arbitrary -- orderContingencyType :: Maybe Text
      <*> arbitrary -- orderExDestination :: Maybe Text
      <*> arbitrary -- orderOrdStatus :: Maybe Text
      <*> arbitrary -- orderTriggered :: Maybe Text
      <*> arbitrary -- orderWorkingIndicator :: Maybe Bool
      <*> arbitrary -- orderOrdRejReason :: Maybe Text
      <*> arbitrary -- orderSimpleLeavesQty :: Maybe Double
      <*> arbitrary -- orderLeavesQty :: Maybe Double
      <*> arbitrary -- orderSimpleCumQty :: Maybe Double
      <*> arbitrary -- orderCumQty :: Maybe Double
      <*> arbitrary -- orderAvgPx :: Maybe Double
      <*> arbitrary -- orderMultiLegReportingType :: Maybe Text
      <*> arbitrary -- orderText :: Maybe Text
      <*> arbitrary -- orderTransactTime :: Maybe DateTime
      <*> arbitrary -- orderTimestamp :: Maybe DateTime

instance Arbitrary OrderBook where
  arbitrary =
    OrderBook
      <$> arbitrary -- orderBookSymbol :: Text
      <*> arbitrary -- orderBookLevel :: Double
      <*> arbitrary -- orderBookBidSize :: Maybe Double
      <*> arbitrary -- orderBookBidPrice :: Maybe Double
      <*> arbitrary -- orderBookAskPrice :: Maybe Double
      <*> arbitrary -- orderBookAskSize :: Maybe Double
      <*> arbitrary -- orderBookTimestamp :: Maybe DateTime

instance Arbitrary OrderBookL2 where
  arbitrary =
    OrderBookL2
      <$> arbitrary -- orderBookL2Symbol :: Text
      <*> arbitrary -- orderBookL2Id :: Double
      <*> arbitrary -- orderBookL2Side :: Text
      <*> arbitrary -- orderBookL2Size :: Maybe Double
      <*> arbitrary -- orderBookL2Price :: Maybe Double

instance Arbitrary Position where
  arbitrary =
    Position
      <$> arbitrary -- positionAccount :: Double
      <*> arbitrary -- positionSymbol :: Text
      <*> arbitrary -- positionCurrency :: Text
      <*> arbitrary -- positionUnderlying :: Maybe Text
      <*> arbitrary -- positionQuoteCurrency :: Maybe Text
      <*> arbitrary -- positionCommission :: Maybe Double
      <*> arbitrary -- positionInitMarginReq :: Maybe Double
      <*> arbitrary -- positionMaintMarginReq :: Maybe Double
      <*> arbitrary -- positionRiskLimit :: Maybe Double
      <*> arbitrary -- positionLeverage :: Maybe Double
      <*> arbitrary -- positionCrossMargin :: Maybe Bool
      <*> arbitrary -- positionDeleveragePercentile :: Maybe Double
      <*> arbitrary -- positionRebalancedPnl :: Maybe Double
      <*> arbitrary -- positionPrevRealisedPnl :: Maybe Double
      <*> arbitrary -- positionPrevUnrealisedPnl :: Maybe Double
      <*> arbitrary -- positionPrevClosePrice :: Maybe Double
      <*> arbitrary -- positionOpeningTimestamp :: Maybe DateTime
      <*> arbitrary -- positionOpeningQty :: Maybe Double
      <*> arbitrary -- positionOpeningCost :: Maybe Double
      <*> arbitrary -- positionOpeningComm :: Maybe Double
      <*> arbitrary -- positionOpenOrderBuyQty :: Maybe Double
      <*> arbitrary -- positionOpenOrderBuyCost :: Maybe Double
      <*> arbitrary -- positionOpenOrderBuyPremium :: Maybe Double
      <*> arbitrary -- positionOpenOrderSellQty :: Maybe Double
      <*> arbitrary -- positionOpenOrderSellCost :: Maybe Double
      <*> arbitrary -- positionOpenOrderSellPremium :: Maybe Double
      <*> arbitrary -- positionExecBuyQty :: Maybe Double
      <*> arbitrary -- positionExecBuyCost :: Maybe Double
      <*> arbitrary -- positionExecSellQty :: Maybe Double
      <*> arbitrary -- positionExecSellCost :: Maybe Double
      <*> arbitrary -- positionExecQty :: Maybe Double
      <*> arbitrary -- positionExecCost :: Maybe Double
      <*> arbitrary -- positionExecComm :: Maybe Double
      <*> arbitrary -- positionCurrentTimestamp :: Maybe DateTime
      <*> arbitrary -- positionCurrentQty :: Maybe Double
      <*> arbitrary -- positionCurrentCost :: Maybe Double
      <*> arbitrary -- positionCurrentComm :: Maybe Double
      <*> arbitrary -- positionRealisedCost :: Maybe Double
      <*> arbitrary -- positionUnrealisedCost :: Maybe Double
      <*> arbitrary -- positionGrossOpenCost :: Maybe Double
      <*> arbitrary -- positionGrossOpenPremium :: Maybe Double
      <*> arbitrary -- positionGrossExecCost :: Maybe Double
      <*> arbitrary -- positionIsOpen :: Maybe Bool
      <*> arbitrary -- positionMarkPrice :: Maybe Double
      <*> arbitrary -- positionMarkValue :: Maybe Double
      <*> arbitrary -- positionRiskValue :: Maybe Double
      <*> arbitrary -- positionHomeNotional :: Maybe Double
      <*> arbitrary -- positionForeignNotional :: Maybe Double
      <*> arbitrary -- positionPosState :: Maybe Text
      <*> arbitrary -- positionPosCost :: Maybe Double
      <*> arbitrary -- positionPosCost2 :: Maybe Double
      <*> arbitrary -- positionPosCross :: Maybe Double
      <*> arbitrary -- positionPosInit :: Maybe Double
      <*> arbitrary -- positionPosComm :: Maybe Double
      <*> arbitrary -- positionPosLoss :: Maybe Double
      <*> arbitrary -- positionPosMargin :: Maybe Double
      <*> arbitrary -- positionPosMaint :: Maybe Double
      <*> arbitrary -- positionPosAllowance :: Maybe Double
      <*> arbitrary -- positionTaxableMargin :: Maybe Double
      <*> arbitrary -- positionInitMargin :: Maybe Double
      <*> arbitrary -- positionMaintMargin :: Maybe Double
      <*> arbitrary -- positionSessionMargin :: Maybe Double
      <*> arbitrary -- positionTargetExcessMargin :: Maybe Double
      <*> arbitrary -- positionVarMargin :: Maybe Double
      <*> arbitrary -- positionRealisedGrossPnl :: Maybe Double
      <*> arbitrary -- positionRealisedTax :: Maybe Double
      <*> arbitrary -- positionRealisedPnl :: Maybe Double
      <*> arbitrary -- positionUnrealisedGrossPnl :: Maybe Double
      <*> arbitrary -- positionLongBankrupt :: Maybe Double
      <*> arbitrary -- positionShortBankrupt :: Maybe Double
      <*> arbitrary -- positionTaxBase :: Maybe Double
      <*> arbitrary -- positionIndicativeTaxRate :: Maybe Double
      <*> arbitrary -- positionIndicativeTax :: Maybe Double
      <*> arbitrary -- positionUnrealisedTax :: Maybe Double
      <*> arbitrary -- positionUnrealisedPnl :: Maybe Double
      <*> arbitrary -- positionUnrealisedPnlPcnt :: Maybe Double
      <*> arbitrary -- positionUnrealisedRoePcnt :: Maybe Double
      <*> arbitrary -- positionSimpleQty :: Maybe Double
      <*> arbitrary -- positionSimpleCost :: Maybe Double
      <*> arbitrary -- positionSimpleValue :: Maybe Double
      <*> arbitrary -- positionSimplePnl :: Maybe Double
      <*> arbitrary -- positionSimplePnlPcnt :: Maybe Double
      <*> arbitrary -- positionAvgCostPrice :: Maybe Double
      <*> arbitrary -- positionAvgEntryPrice :: Maybe Double
      <*> arbitrary -- positionBreakEvenPrice :: Maybe Double
      <*> arbitrary -- positionMarginCallPrice :: Maybe Double
      <*> arbitrary -- positionLiquidationPrice :: Maybe Double
      <*> arbitrary -- positionBankruptPrice :: Maybe Double
      <*> arbitrary -- positionTimestamp :: Maybe DateTime
      <*> arbitrary -- positionLastPrice :: Maybe Double
      <*> arbitrary -- positionLastValue :: Maybe Double

instance Arbitrary Quote where
  arbitrary =
    Quote
      <$> arbitrary -- quoteTimestamp :: DateTime
      <*> arbitrary -- quoteSymbol :: Text
      <*> arbitrary -- quoteBidSize :: Maybe Double
      <*> arbitrary -- quoteBidPrice :: Maybe Double
      <*> arbitrary -- quoteAskPrice :: Maybe Double
      <*> arbitrary -- quoteAskSize :: Maybe Double

instance Arbitrary Settlement where
  arbitrary =
    Settlement
      <$> arbitrary -- settlementTimestamp :: DateTime
      <*> arbitrary -- settlementSymbol :: Text
      <*> arbitrary -- settlementSettlementType :: Maybe Text
      <*> arbitrary -- settlementSettledPrice :: Maybe Double
      <*> arbitrary -- settlementBankrupt :: Maybe Double
      <*> arbitrary -- settlementTaxBase :: Maybe Double
      <*> arbitrary -- settlementTaxRate :: Maybe Double

instance Arbitrary Stats where
  arbitrary =
    Stats
      <$> arbitrary -- statsRootSymbol :: Text
      <*> arbitrary -- statsCurrency :: Maybe Text
      <*> arbitrary -- statsVolume24h :: Maybe Double
      <*> arbitrary -- statsTurnover24h :: Maybe Double
      <*> arbitrary -- statsOpenInterest :: Maybe Double
      <*> arbitrary -- statsOpenValue :: Maybe Double

instance Arbitrary StatsHistory where
  arbitrary =
    StatsHistory
      <$> arbitrary -- statsHistoryDate :: DateTime
      <*> arbitrary -- statsHistoryRootSymbol :: Text
      <*> arbitrary -- statsHistoryCurrency :: Maybe Text
      <*> arbitrary -- statsHistoryVolume :: Maybe Double
      <*> arbitrary -- statsHistoryTurnover :: Maybe Double

instance Arbitrary StatsUSD where
  arbitrary =
    StatsUSD
      <$> arbitrary -- statsUSDRootSymbol :: Text
      <*> arbitrary -- statsUSDCurrency :: Maybe Text
      <*> arbitrary -- statsUSDTurnover24h :: Maybe Double
      <*> arbitrary -- statsUSDTurnover30d :: Maybe Double
      <*> arbitrary -- statsUSDTurnover365d :: Maybe Double
      <*> arbitrary -- statsUSDTurnover :: Maybe Double

instance Arbitrary Trade where
  arbitrary =
    Trade
      <$> arbitrary -- tradeTimestamp :: DateTime
      <*> arbitrary -- tradeSymbol :: Text
      <*> arbitrary -- tradeSide :: Maybe Text
      <*> arbitrary -- tradeSize :: Maybe Double
      <*> arbitrary -- tradePrice :: Maybe Double
      <*> arbitrary -- tradeTickDirection :: Maybe Text
      <*> arbitrary -- tradeTrdMatchId :: Maybe Text
      <*> arbitrary -- tradeGrossValue :: Maybe Double
      <*> arbitrary -- tradeHomeNotional :: Maybe Double
      <*> arbitrary -- tradeForeignNotional :: Maybe Double

instance Arbitrary TradeBin where
  arbitrary =
    TradeBin
      <$> arbitrary -- tradeBinTimestamp :: DateTime
      <*> arbitrary -- tradeBinSymbol :: Text
      <*> arbitrary -- tradeBinOpen :: Maybe Double
      <*> arbitrary -- tradeBinHigh :: Maybe Double
      <*> arbitrary -- tradeBinLow :: Maybe Double
      <*> arbitrary -- tradeBinClose :: Maybe Double
      <*> arbitrary -- tradeBinTrades :: Maybe Double
      <*> arbitrary -- tradeBinVolume :: Maybe Double
      <*> arbitrary -- tradeBinVwap :: Maybe Double
      <*> arbitrary -- tradeBinLastSize :: Maybe Double
      <*> arbitrary -- tradeBinTurnover :: Maybe Double
      <*> arbitrary -- tradeBinHomeNotional :: Maybe Double
      <*> arbitrary -- tradeBinForeignNotional :: Maybe Double

instance Arbitrary Transaction where
  arbitrary =
    Transaction
      <$> arbitrary -- transactionTransactId :: Text
      <*> arbitrary -- transactionAccount :: Maybe Double
      <*> arbitrary -- transactionCurrency :: Maybe Text
      <*> arbitrary -- transactionTransactType :: Maybe Text
      <*> arbitrary -- transactionAmount :: Maybe Double
      <*> arbitrary -- transactionFee :: Maybe Double
      <*> arbitrary -- transactionTransactStatus :: Maybe Text
      <*> arbitrary -- transactionAddress :: Maybe Text
      <*> arbitrary -- transactionTx :: Maybe Text
      <*> arbitrary -- transactionText :: Maybe Text
      <*> arbitrary -- transactionTransactTime :: Maybe DateTime
      <*> arbitrary -- transactionTimestamp :: Maybe DateTime

instance Arbitrary User where
  arbitrary =
    User
      <$> arbitrary -- userId :: Maybe Double
      <*> arbitrary -- userOwnerId :: Maybe Double
      <*> arbitrary -- userFirstname :: Maybe Text
      <*> arbitrary -- userLastname :: Maybe Text
      <*> arbitrary -- userUsername :: Text
      <*> arbitrary -- userEmail :: Text
      <*> arbitrary -- userPhone :: Maybe Text
      <*> arbitrary -- userCreated :: Maybe DateTime
      <*> arbitrary -- userLastUpdated :: Maybe DateTime
      <*> arbitrary -- userPreferences :: Maybe UserPreferences
      <*> arbitrary -- userTfaEnabled :: Maybe Text
      <*> arbitrary -- userAffiliateId :: Maybe Text
      <*> arbitrary -- userPgpPubKey :: Maybe Text
      <*> arbitrary -- userCountry :: Maybe Text

instance Arbitrary UserCommission where
  arbitrary =
    UserCommission
      <$> arbitrary -- userCommissionMakerFee :: Maybe Double
      <*> arbitrary -- userCommissionTakerFee :: Maybe Double
      <*> arbitrary -- userCommissionSettlementFee :: Maybe Double
      <*> arbitrary -- userCommissionMaxFee :: Maybe Double

instance Arbitrary UserPreferences where
  arbitrary =
    UserPreferences
      <$> arbitrary -- userPreferencesAlertOnLiquidations :: Maybe Bool
      <*> arbitrary -- userPreferencesAnimationsEnabled :: Maybe Bool
      <*> arbitrary -- userPreferencesAnnouncementsLastSeen :: Maybe DateTime
      <*> arbitrary -- userPreferencesChatChannelId :: Maybe Double
      <*> arbitrary -- userPreferencesColorTheme :: Maybe Text
      <*> arbitrary -- userPreferencesCurrency :: Maybe Text
      <*> arbitrary -- userPreferencesDebug :: Maybe Bool
      <*> arbitrary -- userPreferencesDisableEmails :: Maybe [Text]
      <*> arbitrary -- userPreferencesHideConfirmDialogs :: Maybe [Text]
      <*> arbitrary -- userPreferencesHideConnectionModal :: Maybe Bool
      <*> arbitrary -- userPreferencesHideFromLeaderboard :: Maybe Bool
      <*> arbitrary -- userPreferencesHideNameFromLeaderboard :: Maybe Bool
      <*> arbitrary -- userPreferencesHideNotifications :: Maybe [Text]
      <*> arbitrary -- userPreferencesLocale :: Maybe Text
      <*> arbitrary -- userPreferencesMsgsSeen :: Maybe [Text]
      <*> arbitrary -- userPreferencesOrderBookBinning :: Maybe A.Value
      <*> arbitrary -- userPreferencesOrderBookType :: Maybe Text
      <*> arbitrary -- userPreferencesOrderClearImmediate :: Maybe Bool
      <*> arbitrary -- userPreferencesOrderControlsPlusMinus :: Maybe Bool
      <*> arbitrary -- userPreferencesSounds :: Maybe [Text]
      <*> arbitrary -- userPreferencesStrictIpCheck :: Maybe Bool
      <*> arbitrary -- userPreferencesStrictTimeout :: Maybe Bool
      <*> arbitrary -- userPreferencesTickerGroup :: Maybe Text
      <*> arbitrary -- userPreferencesTickerPinned :: Maybe Bool
      <*> arbitrary -- userPreferencesTradeLayout :: Maybe Text

instance Arbitrary Wallet where
  arbitrary =
    Wallet
      <$> arbitrary -- walletAccount :: Double
      <*> arbitrary -- walletCurrency :: Text
      <*> arbitrary -- walletPrevDeposited :: Maybe Double
      <*> arbitrary -- walletPrevWithdrawn :: Maybe Double
      <*> arbitrary -- walletPrevTransferIn :: Maybe Double
      <*> arbitrary -- walletPrevTransferOut :: Maybe Double
      <*> arbitrary -- walletPrevAmount :: Maybe Double
      <*> arbitrary -- walletPrevTimestamp :: Maybe DateTime
      <*> arbitrary -- walletDeltaDeposited :: Maybe Double
      <*> arbitrary -- walletDeltaWithdrawn :: Maybe Double
      <*> arbitrary -- walletDeltaTransferIn :: Maybe Double
      <*> arbitrary -- walletDeltaTransferOut :: Maybe Double
      <*> arbitrary -- walletDeltaAmount :: Maybe Double
      <*> arbitrary -- walletDeposited :: Maybe Double
      <*> arbitrary -- walletWithdrawn :: Maybe Double
      <*> arbitrary -- walletTransferIn :: Maybe Double
      <*> arbitrary -- walletTransferOut :: Maybe Double
      <*> arbitrary -- walletAmount :: Maybe Double
      <*> arbitrary -- walletPendingCredit :: Maybe Double
      <*> arbitrary -- walletPendingDebit :: Maybe Double
      <*> arbitrary -- walletConfirmedDebit :: Maybe Double
      <*> arbitrary -- walletTimestamp :: Maybe DateTime
      <*> arbitrary -- walletAddr :: Maybe Text
      <*> arbitrary -- walletScript :: Maybe Text
      <*> arbitrary -- walletWithdrawalLock :: Maybe [Text]

instance Arbitrary XAny where
  arbitrary =

    pure XAny




instance Arbitrary E'Type where
  arbitrary = arbitraryBoundedEnum
