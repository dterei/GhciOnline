{-# LANGUAGE OverloadedStrings #-}
module WarpVersion (main) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.CaseInsensitive as CI
import Data.Char
import Data.Conduit
import Data.Maybe
import Debug.Trace
import qualified Data.IntMap as I
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Responses
import Network.Wai.Router
import Network.Wai.Handler.Warp
import System.IO
import System.Random

type GhciState = MVar (I.IntMap ClientState)

data ClientState = ClientState {
        uid :: Int,
        ghci :: (Handle, Handle, Handle),
        last :: Int
    }

main :: IO ()
main = do
    st <- newMVar I.empty
    run 3001 $ mkRouter $ do
        routeTop (homePage st)
        routeName "index.html" (homePage st)
        routeName "static" $ do
            routeName "fail.png" failpng
            routeName "ok.png" okpng
            routeName "main.js" mainjs
            routeName "style.css" stylecss
        routeName "ghci" $ do
            routeMethod POST (ghciIn st)
        routeAll $ responseLBS status404 [] "Page not found..."

htmlContent, pngContent, jsContent, cssContent :: Header
htmlContent = (hContentType, "text/html")
pngContent  = (hContentType, "image/png")
jsContent   = (hContentType, "application/javascript")
cssContent  = (hContentType, "text/css")

index, failpng, okpng, mainjs, stylecss :: Response
index    = ResponseFile status200 [htmlContent] "static/index.html" Nothing
failpng  = ResponseFile status200 [pngContent] "static/fail.png" Nothing 
okpng    = ResponseFile status200 [pngContent] "static/ok.png" Nothing 
mainjs   = ResponseFile status200 [jsContent] "static/main.js" Nothing
stylecss = ResponseFile status200 [cssContent] "static/style.css" Nothing

homePage :: GhciState -> Application
homePage st req = do
    let cookie = getSession req
    case cookie of
        -- new user
        Nothing -> do
            uid <- liftIO newUID
            return $ ResponseFile status200 [setSession uid, htmlContent] "static/index.html" Nothing

        -- old user
        Just _ -> -- TODO: check session still exists
            return index

ghciIn :: GhciState -> Application
ghciIn _ _ = do
    r <- liftIO newUID
    return $ okHtml (L8.pack $ ".... r: " ++ show r ++ "...")

newUID :: IO Int
newUID = getStdRandom random

setSession :: Int -> Header
setSession uid = ("Set-Cookie", S8.pack $ "SID=" ++ show uid)

getSession :: Request -> Maybe Int
getSession req =
    let cookies = filter (\x -> fst x == hCookie) $ requestHeaders req
        findSids (_,v) = let v1 = S8.dropWhile isSpace v
                             v2 = S8.drop 4 v1
                             n  = S8.readInt v2
                         in case S8.isPrefixOf "SID=" v1 of
                                False -> Nothing
                                True  -> case n of
                                    (Just (x, _)) -> Just x
                                    Nothing       -> Nothing
        sids = catMaybes $ map findSids cookies
    in trace (show $ map show cookies) $ trace (show sids) $ case sids of
            []    -> Nothing
            (x:_) -> Just x

