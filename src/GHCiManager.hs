module GHCiManager where

import Data.List (isInfixOf)
import System.IO
import System.Process

import GHCiParser

type GHCiHandle = (Handle, Handle, Handle)

ghciPath :: FilePath
ghciPath = "ghci"

ghciArgs :: [String]
ghciArgs = []

stdoutSentinel, stderrSentinel :: String
stdoutSentinel = "01234568909876543210"
stderrSentinel = "oopsthisisnotavariable"

newGHCi :: IO GHCiHandle
newGHCi = do
    (Just hin, Just hout, Just herr, _) <-
      createProcess (proc ghciPath ghciArgs) {
              std_out = CreatePipe, std_in = CreatePipe, std_err = CreatePipe
          }
    hSetBuffering hin NoBuffering
    hSetBuffering hout NoBuffering
    hSetBuffering herr NoBuffering
    hPutStr hin $ ":t " ++ stdoutSentinel ++ "\n"
    _ <- getGHCiOut hout
    return (hin, hout, herr)

queryGHCI :: GHCiHandle -> String -> IO String
queryGHCI (hin, hout, herr) input = do
    putStrLn $ "query ghci: "  ++ input
    hPutStr hin $ ensureNewLine input
    -- This is a hack that lets us discover where the end of the output is.
    -- We will keep reading until we see the sentinel.
    errors <- do
        hPutStr hin $ stderrSentinel ++ "\n"
        getGHCiErr herr
    output <- do
        hPutStr hin $ ":t " ++ stdoutSentinel ++ "\n"
        getGHCiOut hout
    if trimWhitespace errors == "" 
        then return output
        else return $ "ERR: " ++ show (parseErrors errors)

getGHCiErr :: Handle -> IO String
getGHCiErr herr' = 
    go herr' ""
  where
    go herr results = do
      line <- hGetLine herr
      -- putStrLn $ "Error: " ++ line
      if stderrSentinel `isInfixOf` line
        then return(results)
        else go herr (results ++ "\n" ++ line)

getGHCiOut :: Handle -> IO String
getGHCiOut hout = go []
  where
    go acc = do
      l <- hGetLine hout
      -- putStrLn $ "Input: " ++ l
      if stdoutSentinel `isInfixOf` l
        then return $ done acc
        else go (l:acc)
    
    done [] = "\n"
    done xs = unlines $ reverse xs

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

