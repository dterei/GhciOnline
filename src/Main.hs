{-# LANGUAGE OverloadedStrings, PatternGuards #-}
-- | GHCi Online entry point.
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
import System.IO

import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe

import GHCiManager
import Sessions
import State
import qualified Timeout as T

-- | Snap server configuration.
config :: Config Snap ()
config = setPort 3222 mempty

-- | Main entry point.
main :: IO ()
main = do
    st <- initState
    hFlush stdout
    httpServe config $ site st

-- | Routes for ghci online.
site :: GhciState -> Snap ()
site gst = do
    req <- getRequest
    routes req
  where
    indexFile = (gsHtmlRoot gst) ++ "/index.html"
    routes req =
        if (rqServerName req == "www.ghc.io") then (redirect' "http://ghc.io" 301) else pass <|>
        ifTop (index gst indexFile) <|>
        path "ghci" (method POST (ghciIn gst)) <|>
        dir "static" (serveDirectoryWith conf $ gsHtmlRoot gst)

    -- somewhat of a hack to get UTF-8 info passed in headers...
    utf8mime = H.map (\v -> v `S8.append` "; charset=UTF-8") defaultMimeTypes
    conf = simpleDirectoryConfig { mimeTypes = utf8mime }

-- | Server index page.
index :: GhciState -> String -> Snap ()
index gst file = do
    uid <- getSession <|> newSession (gsClients gst)
    startSession gst uid
    modifyResponse $ setContentType "text/html; charset=UTF-8"
    modifyResponse $ setHeader "Content-Language" "en"
    sendFile file

-- | Start a new GHCi session for a user.
startSession :: GhciState -> UID -> Snap ()
startSession gst uid = liftIO $ modifyMVar_ (gsClients gst) $ \st ->
    case I.lookup uid st of
        Just _  -> return st
        Nothing -> do
            h <- liftIO $ newGHCi gst
            t <- T.register (gsTimeout gst) $ endSession gst uid
            return $ I.insert uid (ClientState h t) st

-- | End a GHCi session for a user.
endSession :: GhciState -> UID -> IO ()
endSession gst uid = modifyMVar_ (gsClients gst) $ \st -> do
    -- lookup + delete in one operation
    let (mclient, st') = I.updateLookupWithKey (\_ _ -> Nothing) uid st
    case mclient of
        Nothing -> return st'
        Just client -> do
            killGHCi $ csGhci client
            T.cancel $ csTout client
            return st'

-- | Process some input from the user.
ghciIn :: GhciState -> Snap ()
ghciIn gst = do
    (uid, cst) <- requireSession (gsClients gst)
    liftIO $ T.tickle (csTout cst)
    uin <- getData
    liftIO $ T.pause (csTout cst)
    out' <- liftIO $ queryGHCi (csGhci cst) (decodeUtf8 uin)
    out <- case out' of
        Left  out -> liftIO (endSession gst uid) >> return out
        Right out -> return out
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

