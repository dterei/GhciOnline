{-# LANGUAGE OverloadedStrings #-}
module GHCiManager (
        GHCiHandle,
        newGHCi,
        killGHCi,
        queryGHCi
    ) where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import System.Process

import GHCiParser

type GHCiHandle = (Handle, Handle, Handle, ProcessHandle)

ghciPath :: FilePath
ghciPath = "/home/hs15/davidt/ghci-safe/dist/build/ghci-safe/ghci-safe"

ghciArgs :: [String]
ghciArgs = ["-XSafe", "-fpackage-trust", "-distrust-all-packages", "-trust base"]

stdoutSentinel, stderrSentinel :: Text
stdoutSentinel = "01234568909876543210"
stderrSentinel = "oopsthisisnotavariable"

newGHCi :: IO GHCiHandle
newGHCi = do
    (Just hin, Just hout, Just herr, pid) <-
      createProcess (proc ghciPath ghciArgs) {
              std_out = CreatePipe, std_in = CreatePipe, std_err = CreatePipe
          }
    hSetBuffering hin NoBuffering
    hSetBuffering hout NoBuffering
    hSetBuffering herr NoBuffering
    T.hPutStrLn hin $ ":t " `T.append` stdoutSentinel
    _ <- getGHCiOut hout stdoutSentinel
    T.hPutStrLn hin $ stderrSentinel
    _ <- getGHCiOut herr stderrSentinel
    clearHandle hout
    return (hin, hout, herr, pid)

killGHCi :: GHCiHandle -> IO ()
killGHCi (_, _, _, pid) = terminateProcess pid

queryGHCi :: GHCiHandle -> Text -> IO Text
queryGHCi (hin, hout, herr, _) input = do
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
    if T.null errors
        then return $ prepJSON "value" prompt (jsonText output)
        else return $ prepJSON "error" "" (parseErrors errors)
  where
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

