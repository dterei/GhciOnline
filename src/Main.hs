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

type GhciState = Session ClientState

data ClientState = ClientState {
        csGhci :: GHCiHandle,
        csTout :: T.Handle
    }

config :: Config Snap ()
config = setPort 3001 mempty

main :: IO ()
main = do
    st <- newMVar I.empty
    httpServe config (site st)

site :: GhciState -> Snap ()
site st = 
    ifTop (index st) <|>
    path "ghci" (method POST (ghciIn st)) <|>
    dir "static" (serveDirectoryWith conf "static")
  where
    -- somewhat of a hack to get UTF-8 info passed in headers...
    utf8mime = H.map (\v -> v `S8.append` "; charset=UTF-8") defaultMimeTypes
    conf = simpleDirectoryConfig { mimeTypes = utf8mime }

index :: GhciState -> Snap ()
index mst = do
    uid <- getSession <|> newSession mst
    startSession mst uid
    modifyResponse $ setContentType "text/html; charset=UTF-8"
    modifyResponse $ setHeader "Content-Language" "en"
    sendFile "static/index.html"

startSession :: GhciState -> UID -> Snap ()
startSession mst uid = liftIO $ modifyMVar_ mst $ \st ->
    case I.lookup uid st of
        Just _  -> return st
        Nothing -> do
            h <- liftIO newGHCi
            -- TODO: have actual timeout manager...
            t <- T.register undefined $ endSession mst uid
            return $ I.insert uid (ClientState h t) st

endSession :: GhciState -> UID -> IO ()
endSession mst uid = modifyMVar_ mst $ \st ->
    -- lookup + delete in one operation
    let (mclient, st') = I.updateLookupWithKey (\_ _ -> Nothing) uid st
    in case mclient of
        Nothing -> return st'
        Just client -> do
            killGHCi $ csGhci client
            T.cancel $ csTout client
            return st'

ghciIn :: GhciState -> Snap ()
ghciIn mst = do
    cst <- requireSession mst
    uin <- getData
    out <- liftIO $ queryGHCi (csGhci cst) (decodeUtf8 uin)
    writeBS $ encodeUtf8 out
    modifyResponse $ setContentType "text/plain; charset=UTF-8"
  where
    getData = do
        input <- getPostParam "data"
        case input of
            Just d -> return d
            _ -> do
                writeBS "Error: You must include a 'data' post parameter!"
                r <- getResponse
                finishWith $ setResponseCode 400 r

