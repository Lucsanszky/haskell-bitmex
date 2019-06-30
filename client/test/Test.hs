{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Typeable (Proxy(..))

import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.HUnit

import Data.Text                (pack)
import Data.Aeson               (decode)
import Data.Time.Clock.POSIX    (getPOSIXTime)
import Network.HTTP.Client      (newManager)
import Network.HTTP.Client.TLS  (tlsManagerSettings)

import qualified Network.HTTP.Client.Internal as NH
import qualified Network.HTTP.Types           as NH
import           Data.IORef

import BitMEX

import           BitMEXClient hiding (error, Position, Order)
import qualified BitMEXClient as BMC (Symbol(..))

----------------------------------------
newtype API_ID     = API_ID      String deriving (Show, Eq)
newtype API_SECRET = API_SECRET  String deriving Show

instance IsOption Environment where
    defaultValue = error "User must specify MainNet/TestNet Environment (on command line or environment) for authenticated tests."
    parseValue x
      | x == "MainNet" = Just MainNet
      | x == "TestNet" = Just TestNet
      | otherwise      = Just defaultValue

    optionName = return "BITMEX_ENVIRONMENT"  -- env: prepend 'TASTY_'
    optionHelp = return "Which BitMEX environment (MainNet or TestNet) to use to run the tests."

instance IsOption API_ID where
    defaultValue = error "User must supply BITMEX_API_ID (on command line or environment) for authenticated tests."
    parseValue = Just . API_ID
    optionName = return "BITMEX_API_ID"
    optionHelp = return "Customer's API ID for Bitmex account (hex encoded)."

instance IsOption API_SECRET where
    defaultValue = error "User must supply BITMEX_API_SECRET (on command line or environment) for authenticated tests."
    parseValue = Just . API_SECRET
    optionName = return "BITMEX_API_SECRET" -- export TASTY_BITMEX_API_SECRET=...
    optionHelp = return "Customer's API secret for Bitmex account (hex encoded)."


main :: IO ()
main = defaultMainWithIngredients ings $
    askOption $ \env ->
    askOption $ \apiid ->
    askOption $ \apikey ->
    withResource (mkConfig env apiid apikey) (\_ -> return ()) (\config -> tests config)
  where
    ings = includingOptions
        [ (Option (Proxy :: Proxy Environment))
        , (Option (Proxy :: Proxy API_ID))
        , (Option (Proxy :: Proxy API_SECRET))
        ] : defaultIngredients

    mkConfig :: Environment -> API_ID -> API_SECRET -> IO BitMEX
    mkConfig env (API_ID apiid) (API_SECRET apikey) = do
        mgr  <- newManager tlsManagerSettings
        logs <- initLogContext -- >>= stderrLevelLoggingContext levelDebug
        return $ BitMEX
            { netEnv      = env
            , restPath    = "/api/v1"
            , wsPath      = "/realtime"
            , connManager = mgr
            , apiCreds    = APICreds {apiId = apiid, apiSecret = apikey}
            , logConfig   = logs
            }

tests :: IO BitMEX -> TestTree
tests config = testGroup "" [instanceProps, unitTests config]

unitTests :: IO BitMEX -> TestTree
unitTests config = testGroup "\nAPI unit tests"
    [ testCase "Change leverage" $ do
        let leverageRequest =
                positionUpdateLeverage
                    (ContentType MimeFormUrlEncoded)
                    (Accept MimeJSON)
                    (Symbol $ (pack . show) BMC.XBTUSD)
                    (Leverage 8.0)

        bitmex   <- config
        response <- dispatchRequest bitmex leverageRequest
        case mimeResult response of
            Right (Position {}) -> return ()
            _                   -> assertFailure $ "Unable to update leverage:" <> show response

    -- CAREFUL: This testCase will use (TestNet or Real) funds
    -- Don't give it credentials that can make you lose real money, use the TestNet
    , testCase "Place and cancel order" $ do
        bitmex <- config
        time   <- getPOSIXTime

        let clientOrderId = "TEST--ClientOrdID-" <> show time -- must be unique
            placeNewOrder =
                orderNew
                    (ContentType MimeFormUrlEncoded)
                    (Accept MimeJSON)
                    (Symbol $ (pack . show) BMC.XBTUSD)
                    -&- (OrderQty  40)
                    -&- (Price   1000)
                    -&- (ClOrdId $ pack clientOrderId)
            cancelOrder =
                orderCancel
                    (ContentType MimeFormUrlEncoded)
                    (Accept MimeJSON)
                    -&- (ClOrdId $ pack clientOrderId)

        response <- dispatchRequest bitmex placeNewOrder
        _ <- case mimeResult response of
            Right (Order {}) -> return ()
            _ -> assertFailure $ "Unable to place order: " <> show response

        response' <- dispatchRequest bitmex cancelOrder
        case mimeResult response' of
            Right [Order {}] -> return ()
            _ -> assertFailure $ "Unable to cancel order: " <> show response

    , testCase "Websocket parse funding fee" $ do
        case decode sampleFundingExecutionMsg :: Maybe Response of
            Just _ -> return ()
            Nothing -> assertFailure "Could not parse funding fee execution message."

    , testCase "Multiple retries upon 503 HTTP Status" $ do
        ref   <- newIORef 0
        resp  <- retryOn503 9 (fakeDispatch ref)
        count <- readIORef ref
        assertEqual "retry response status does not match action status"
            (NH.responseStatus $ mimeResultResponse sampleTooBusyResponse)
            (NH.responseStatus $ mimeResultResponse resp)
        assertEqual "Retried wrong number of times" 10 count
    ]

instanceProps :: TestTree
instanceProps = testGroup "Properties" []

--------------------------------------------------------------------------------
sampleFundingExecutionMsg =
    "{\"table\":\"execution\",\"action\":\"insert\",\"data\":[{\"execID\":\"a747f8fb-b420-2564-ee83-50715b574eb2\",\"orderID\":"
    <> "\"00000000-0000-0000-0000-000000000000\",\"clOrdID\":\"\",\"clOrdLinkID\":\"\",\"account\":517257,\"symbol\":\"XBTUSD\","
    <> "\"side\":\"\",\"lastQty\":14,\"lastPx\":10571.37,\"underlyingLastPx\":null,\"lastMkt\":\"XBME\",\"lastLiquidityInd\":\"\""
    <> ",\"simpleOrderQty\":null,\"orderQty\":14,\"price\":10571.37,\"displayQty\":null,\"stopPx\":null,\"pegOffsetValue\":null,"
    <> "\"pegPriceType\":\"\",\"currency\":\"USD\",\"settlCurrency\":\"XBt\",\"execType\":\"Funding\",\"ordType\":\"Limit\","
    <> "\"timeInForce\":\"AtTheClose\",\"execInst\":\"\",\"contingencyType\":\"\",\"exDestination\":\"XBME\",\"ordStatus\":"
    <> "\"Filled\",\"triggered\":\"\",\"workingIndicator\":false,\"ordRejReason\":\"\",\"simpleLeavesQty\":null,\"leavesQty\":0,"
    <> "\"simpleCumQty\":null,\"cumQty\":14,\"avgPx\":10571.37,\"commission\":0.002832,\"tradePublishIndicator\":\"\","
    <> "\"multiLegReportingType\":\"SingleSecurity\",\"text\":\"Funding\",\"trdMatchID\":\"716fe240-bdee-29da-ab67-6aaf02698a0d\","
    <> "\"execCost\":-132440,\"execComm\":375,\"homeNotional\":0.0013244,\"foreignNotional\":-14,"
    <> "\"transactTime\":\"2019-06-23T12:00:00.000Z\",\"timestamp\":\"2019-06-23T12:00:01.438Z\"}]}"

--------------------------------------------------------------------------------
sampleTooBusyResponse :: MimeResult res
sampleTooBusyResponse =  MimeResult
    { mimeResult = Left
        ( MimeError
            { mimeError = "error statusCode: 503"
            , mimeErrorResponse = NH.Response
                { NH.responseStatus = NH.Status
                    { NH.statusCode = 503
                    , NH.statusMessage = "Service Unavailable"
                    }
                , NH.responseVersion = NH.http11
                , NH.responseHeaders = []
                , NH.responseBody = "{\"error\":{\"message\":\"The system is currently overloaded. Please try again later.\",\"name\":\"HTTPError\"}}"
                , NH.responseCookieJar = NH.CJ {NH.expose = []}
                , NH.responseClose' = undefined
                }
            }
        )
    , mimeResultResponse = NH.Response
        { NH.responseStatus = NH.Status
            { NH.statusCode = 503
            , NH.statusMessage = "Service Unavailable"
            }
        , NH.responseVersion = NH.http11
        , NH.responseHeaders = []
        , NH.responseBody = "{\"error\":{\"message\":\"The system is currently overloaded. Please try again later.\",\"name\":\"HTTPError\"}}"
        , NH.responseCookieJar = NH.CJ {NH.expose = []}
        , NH.responseClose' = undefined
        }
    }

fakeDispatch :: IORef Int -> IO (MimeResult ())
fakeDispatch ref = do
    modifyIORef ref (+1)
    return sampleTooBusyResponse
--------------------------------------------------------------------------------
