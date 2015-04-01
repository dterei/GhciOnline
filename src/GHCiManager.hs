{-# LANGUAGE OverloadedStrings #-}
module GHCiManager (
        GHCiHandle,
        newGHCi,
        killGHCi,
        queryGHCi
    ) where

import CJail.System.Process
import Control.Exception
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import System.IO.Error

import GHCiParser
import State

stdoutSentinel, stderrSentinel :: Text
stdoutSentinel = "01234568909876543210"
stderrSentinel = "oopsthisisnotavariable"

newGHCi :: GhciState -> IO GHCiHandle
newGHCi gst = flip catchIOError ioHandle $ do
    phandle@(ProcessHandle hin hout herr _) <-
      createProcess (gsCJail gst) $
        proc (gsGhciPath gst) (gsGhciArgs gst)
    hSetBuffering hin NoBuffering
    hSetBuffering hout NoBuffering
    hSetBuffering herr NoBuffering
    T.hPutStrLn hin $ ":t " `T.append` stdoutSentinel
    _ <- getGHCiOut hout stdoutSentinel
    T.hPutStrLn hin $ stderrSentinel
    _ <- getGHCiOut herr stderrSentinel
    clearHandle hout
    return phandle

  where
    ioHandle err = do
      when (isEOFError err) $ do
        putStrLn "Couldn't launch GHCi!"
      ioError err

killGHCi :: GHCiHandle -> IO ()
killGHCi = terminateProcess

queryGHCi :: GHCiHandle -> Text -> IO (Either Text Text)
queryGHCi (ProcessHandle hin hout herr _) input = do
    failGHCi `tryJust` toGHCi
  where
    toGHCi :: IO Text
    toGHCi = do
        clearHandle hout
        T.hPutStrLn hin $ ensureNoNewLine input
        -- This is a hack that lets us discover where the end of the output is.
        -- We will keep reading until we see the sentinel.
        (prompt, output) <- do
            T.hPutStrLn hin $ ":t " `T.append` stdoutSentinel
            o <- getGHCiOut hout stdoutSentinel
            p <- hGetAvailable hout
            return $ (T.strip p, T.strip o)
        errors <- do
            T.hPutStrLn hin $ stderrSentinel
            o <- getGHCiOut herr stderrSentinel
            return $ T.strip o
        return $ if T.null errors
            then prepJSON "value" prompt (jsonText output)
            else prepJSON "error" "" (parseErrors errors)

    failGHCi :: SomeException -> Maybe Text
    failGHCi _ = Just $ prepJSON "fatal" "" $ parseErrors $
        "::An irrecoverable error occured with GHCi!\n" `T.append`
        "We currently only allow a session to use up to 100MB " `T.append`
        "of memory.\nPlease reload the website to restart..."

    prepJSON :: Text -> Text -> Text -> Text
    prepJSON t p l = "{" `T.append`
            "\"prompt\":\"" `T.append` p `T.append`
            "\", \"type\":\"" `T.append` t `T.append`
            "\", \"msg\":" `T.append` l `T.append`
        "}" 

getGHCiOut :: Handle -> Text -> IO Text
getGHCiOut h sentinel = go []
  where
    go acc = do
      l <- T.hGetLine h
      if sentinel `T.isInfixOf` l
        then return (done acc)
        else go (l:acc)
    
    done [] = "\n"
    done xs = T.unlines $ reverse xs

clearHandle :: Handle -> IO ()
clearHandle = void . hGetAvailable

hGetAvailable :: Handle -> IO Text
hGetAvailable h = go ""
  where
    go acc = do
        -- this delay is arbitrary, but we need something
        r <- hWaitForInput h 5
        case r of
            True  -> hGetChar h >>= \c -> go (acc `T.snoc` c)
            False -> return acc

-- hGetBlockInitial :: Handle -> IO String
-- hGetBlockInitial h = do
--     l <- hGetLine h
--     putStrLn $ "Input: " ++ l
--     ls <- hGetAvailable h
--     if null ls
--         then return l
--         else return $ l ++ "\n" ++ ls

