{-# LANGUAGE OverloadedStrings #-}
module GHCiManager (
        GHCiHandle,
        newGHCi,
        killGHCi,
        queryGHCi
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import System.Process

import GHCiParser

type GHCiHandle = (Handle, Handle, Handle, ProcessHandle)

ghciPath :: FilePath
ghciPath = "ghci"

ghciArgs :: [String]
ghciArgs = ["-XSafe", "-fpackage-trust"]

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
    return (hin, hout, herr, pid)

killGHCi :: GHCiHandle -> IO ()
killGHCi (_, _, _, pid) = terminateProcess pid

queryGHCi :: GHCiHandle -> Text -> IO Text
queryGHCi (hin, hout, herr, _) input = do
    T.hPutStrLn hin $ ensureNoNewLine input
    -- This is a hack that lets us discover where the end of the output is.
    -- We will keep reading until we see the sentinel.
    errors <- do
        T.hPutStrLn hin $ stderrSentinel
        cont <- getGHCiOut herr stderrSentinel
        return $ T.strip cont
    output <- do
        T.hPutStrLn hin $ ":t " `T.append` stdoutSentinel
        cont <- getGHCiOut hout stdoutSentinel
        return $ T.strip cont
    if T.null errors
        then do
            let (p, l) = splitPrompt output
            return $ prepJSON "value" p (jsonText l)
        else return $ prepJSON "error" "" (parseErrors errors)
  where
    prepJSON t p l = "{" `T.append`
            "\"prompt\":\"" `T.append` p `T.append`
            "\", \"type\":\"" `T.append` t `T.append`
            "\", \"msg\":" `T.append` l `T.append`
        "}" 

getGHCiOut :: Handle -> Text -> IO Text
getGHCiOut hout sentinel = go []
  where
    go acc = do
      l <- T.hGetLine hout
      if sentinel `T.isInfixOf` l
        then return (done acc)
        else go (l:acc)
    
    done [] = "\n"
    done xs = T.unlines $ reverse xs

-- hGetBlockInitial :: Handle -> IO String
-- hGetBlockInitial h = do
--     l <- hGetLine h
--     putStrLn $ "Input: " ++ l
--     ls <- hGetAvailable h
--     if null ls
--         then return l
--         else return $ l ++ "\n" ++ ls

-- hGetAvailable :: Handle -> IO String
-- hGetAvailable h = go ""
--   where
--     go acc = do
--         r <- hReady h
--         case r of
--             True  -> hGetChar h >>= \c -> go (c:acc)
--             False -> return $ reverse acc

