{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Main (
        main
    ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as S8
import qualified Data.IntMap as I
import Data.Monoid
import qualified Data.HashMap.Strict as H
import Data.Text.Encoding

import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe

import GHCiManager
import Sessions
import qualified Timeout as T

data GhciState = GhciState {
        gsClients :: Session ClientState,
        gsTimeout :: T.Manager
    }

data ClientState = ClientState {
        csGhci :: GHCiHandle,
        csTout :: T.Handle
    }

config :: Config Snap ()
config = setPort 3222 mempty

main :: IO ()
main = do
    st <- newMVar I.empty
    -- 15 seconds...
    tm <- T.initialize (15 * 1000000)
    httpServe config (site $ GhciState st tm)

site :: GhciState -> Snap ()
site gst = do
    req <- getRequest
    routes req
  where
    routes req =
        if (rqServerName req == "www.ghc.io") then (redirect' "http://ghc.io" 301) else pass <|>
        ifTop (index gst "static/index2.html") <|>
        path "ghci" (method POST (ghciIn gst)) <|>
        dir "static" (serveDirectoryWith conf "static")
    -- somewhat of a hack to get UTF-8 info passed in headers...
    utf8mime = H.map (\v -> v `S8.append` "; charset=UTF-8") defaultMimeTypes
    conf = simpleDirectoryConfig { mimeTypes = utf8mime }

index :: GhciState -> String -> Snap ()
index gst file = do
    uid <- getSession <|> newSession (gsClients gst)
    startSession gst uid
    modifyResponse $ setContentType "text/html; charset=UTF-8"
    modifyResponse $ setHeader "Content-Language" "en"
    sendFile file

startSession :: GhciState -> UID -> Snap ()
startSession gst uid = liftIO $ modifyMVar_ (gsClients gst) $ \st ->
    case I.lookup uid st of
        Just _  -> return st
        Nothing -> do
            h <- liftIO newGHCi
            t <- T.register (gsTimeout gst) $ endSession gst uid
            -- let t = undefined
            return $ I.insert uid (ClientState h t) st

endSession :: GhciState -> UID -> IO ()
endSession gst uid = modifyMVar_ (gsClients gst) $ \st ->
    -- lookup + delete in one operation
    let (mclient, st') = I.updateLookupWithKey (\_ _ -> Nothing) uid st
    in case mclient of
        Nothing -> return st'
        Just client -> do
            killGHCi $ csGhci client
            T.cancel $ csTout client
            return st'

ghciIn :: GhciState -> Snap ()
ghciIn gst = do
    cst <- requireSession (gsClients gst)
    liftIO $ T.tickle (csTout cst)
    uin <- getData
    liftIO $ T.pause (csTout cst)
    out <- liftIO $ queryGHCi (csGhci cst) (decodeUtf8 uin)
    liftIO $ T.resume (csTout cst)
    writeBS $ encodeUtf8 out
    modifyResponse $ setContentType "text/plain; charset=UTF-8"
  where
    getData = do
        input <- getPostParam "data"
        case input of
            Just d -> return d
            _ -> do
                -- treat empty post as a keep-alive message
                r <- getResponse
                finishWith $ r

