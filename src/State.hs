-- | State associated with user or application.
module State (
        GHCiHandle, GhciState(..), ClientState(..), initState
    ) where

import CJail.System.Process
import Control.Concurrent.MVar
import Data.Functor
import Data.List
import qualified Data.IntMap as I
import System.Environment

import Sessions
import qualified Timeout as T

type GHCiHandle = ProcessHandle

-- global application state.
data GhciState = GhciState {
        gsClients  :: Session ClientState,
        gsTimeout  :: T.Manager,
        gsCJail    :: CJailConf,
        gsGhciPath :: FilePath,
        gsGhciArgs :: [String],
        gsHtmlRoot :: String
    }

-- Session data for a single user.
data ClientState = ClientState {
        csGhci :: GHCiHandle,
        csTout :: T.Handle
    }

initState :: IO GhciState
initState = do
    st <- newMVar I.empty
    tm <- T.initialize (15 * 1000000)
    cjailPath <- getEnv "CJAIL_PATH"
    let cj = CJailConf Nothing Nothing cjailPath
    ghciPath <- getEnv "GHCI_PATH"
    ghciArgs <- splitOn ',' <$> getEnv "GHCI_ARGS"
    htmlRoot <- getEnv "HTML_ROOT"

    putStrLn $ "CJAIL_PATH: " ++ cjailPath
    putStrLn $ "GHCI_PATH: " ++ ghciPath
    putStrLn $ "GHCI_ARGS: " ++ show ghciArgs
    putStrLn $ "HTML_ROOT: " ++ htmlRoot

    return $ GhciState st tm cj ghciPath ghciArgs htmlRoot

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn chr = unfoldr sep where
  sep [] = Nothing
  sep l  = Just . fmap (drop 1) . break (==chr) $ l

