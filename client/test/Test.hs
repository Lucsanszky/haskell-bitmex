{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))

import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.HUnit

import Data.Text                (pack)
import Network.HTTP.Client      (newManager)
import Network.HTTP.Client.TLS  (tlsManagerSettings)

import BitMEX
          ( ContentType(..)
          , MimeJSON(..)
          , Accept(..)
          , Symbol(..)
          , Leverage(..)
          , positionUpdateLeverage
          , initLogContext
          , stderrLevelLoggingContext
          , levelDebug
          , MimeFormUrlEncoded(..)
          , mimeResultResponse
          )

import           BitMEXClient hiding (error)
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

    optionName = return "BITMEX_ENVIRONMENT"
    optionHelp = return "Which BitMEX environment (MainNet or TestNet) to use to run the tests."

instance IsOption API_ID where
    defaultValue = error "User must supply BITMEX_API_ID (on command line or environment) for authenticated tests."
    parseValue = Just . API_ID
    optionName = return "BITMEX_API_ID"
    optionHelp = return "Customer's API ID for Bitmex account (hex encoded)."

instance IsOption API_SECRET where
    defaultValue = error "User must supply BITMEX_API_SECRET (on command line or environment) for authenticated tests."
    parseValue = Just . API_SECRET
    optionName = return "BITMEX_API_SECRET"
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
        mgr <- newManager tlsManagerSettings
        log <- initLogContext -- >>= stderrLevelLoggingContext levelDebug
        return $ BitMEX
            { netEnv      = env
            , restPath    = "/api/v1"
            , wsPath      = "/realtime"
            , connManager = mgr
            , apiCreds    = APICreds {apiId = apiid, apiSecret = apikey}
            , logConfig   = log
            }

tests :: IO BitMEX -> TestTree
tests config = testGroup "" [instanceProps, unitTests config]

unitTests :: IO BitMEX -> TestTree
unitTests config = testGroup "\nAPI unit tests"
  [ testCase "Show API credentials and change leverage" $ do
        bitmex <- config
        print $ netEnv   bitmex
        print $ apiCreds bitmex

        let leverageRequest =
                positionUpdateLeverage
                    (ContentType MimeFormUrlEncoded)
                    (Accept MimeJSON)
                    (Symbol ((pack . show) BMC.XBTUSD))
                    (Leverage 8.0)

        res <- dispatchRequest bitmex leverageRequest

        print (mimeResultResponse res)
        return ()
  ]

instanceProps :: TestTree
instanceProps = testGroup "Properties" []
