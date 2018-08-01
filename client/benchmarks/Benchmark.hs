module Main where

import qualified BitMEX                  as Mex
import           BitMEXClient
import qualified Control.Monad.Reader    as R
import           Criterion.Main
import           Data.Aeson
import           Data.ByteString         (readFile)
import           Data.Monoid
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Prelude
    ( Maybe (..)
    , return
    , show
    , ($)
    , (.)
    )
import           System.Environment      (getArgs)

updateLeverage ::
       Symbol
    -> Mex.Leverage
    -> BitMEXReader (Mex.MimeResult Mex.Position)
updateLeverage sym lev = do
    let leverageTemplate =
            Mex.positionUpdateLeverage
                (Mex.ContentType Mex.MimeJSON)
                (Mex.Accept Mex.MimeJSON)
                (Mex.Symbol ((T.pack . show) sym))
                lev
        leverageRequest =
            Mex._setBodyLBS leverageTemplate $
            "{\"leverage\": " <> encode (Mex.unLeverage lev) <>
            ", \"symbol\": " <>
            encode ((T.pack . show) sym) <>
            "}"
    makeRequest leverageRequest

main = do
    mgr <- newManager tlsManagerSettings
    (pubPath:privPath:_) <- getArgs
    pub <- T.readFile pubPath
    priv <- readFile privPath
    logCxt <- Mex.initLogContext
    let config =
            BitMEXWrapperConfig
            { environment = TestNet
            , pathREST = Just "/api/v1"
            , pathWS = Just "/realtime"
            , manager = Just mgr
            , publicKey = pub
            , privateKey = priv
            , logExecContext =
                  Mex.runDefaultLogExecWithContext
            , logContext = logCxt
            }
    defaultMain
        [ bench "post leverage" $
          nfIO
              (R.runReaderT
                   (run (do r <-
                                updateLeverage
                                    XBTUSD
                                    (Mex.Leverage 3)
                            return ()))
                   config)
        ]
