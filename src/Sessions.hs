{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Sessions (
        UID, newUID, getUID,
        Session, newSession, expireSession, getSession, requireSession
    ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as S8
import qualified Data.IntMap as I
import System.Random

import Snap.Core

type UID = Int

type Session a = MVar (I.IntMap a)

sessionCookie :: Cookie
sessionCookie = Cookie "_session" "" Nothing Nothing Nothing False False

newUID :: Session a -> IO UID
newUID mst = do
    uid <- getStdRandom random
    exists <- liftIO $ withMVar mst (return . I.member uid)
    if exists
        then newUID mst
        else return uid

getUID :: Snap (Maybe UID)
getUID = do
    sid <- getCookie $ cookieName sessionCookie
    case sid of
        Nothing -> logError "No session cookie found" >> return Nothing
        Just c | Just (uid, _) <- S8.readInt (cookieValue c) -> return (Just uid)
        Just _ -> logError "session found but not an int!" >> return Nothing

newSession :: Session a -> Snap Int
newSession mst = do
    uid <- liftIO $ newUID mst
    modifyResponse $ addResponseCookie $
        sessionCookie { cookieValue = (S8.pack $ show uid) }
    return uid

expireSession :: Snap ()
expireSession =
    expireCookie (cookieName sessionCookie) (cookieDomain sessionCookie)

getSession :: Snap UID
getSession = do
    uid' <- getUID
    case uid' of
        Nothing  -> pass
        Just uid -> return uid

requireSession :: Session a -> Snap a
requireSession mst = do
    uid <- getSession <|> noSessionError
    c'  <- liftIO $ withMVar mst (return . I.lookup uid)
    case c' of
        Just c  -> return c
        Nothing -> do
            -- invalid session cookie so expire it
            expireSession
            noSessionError
  where
    noSessionError = do
        writeBS "Error: You must start a session before using ghci!"
        r <- getResponse
        finishWith $ setResponseCode 400 r

