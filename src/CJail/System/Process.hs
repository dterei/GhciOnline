{- |

This module exports an interface that is analogous to
"System.Process", with the following differences:

* All the commands and executables are withing a @cjail@ and
  so peristance staorage lasts only for the duration of a 'runCJail'.

* Neither the environment nor file handles of the current process
  are inherited. New file handles are always created, and the 
  environment is by default empty.

-}

module CJail.System.Process ( -- * Running sub-processes
                              createProcess
                            , shell, proc
                            , CJailConf(..)
                            , CreateProcess(..)
                            , CmdSpec(..)
                            , ProcessHandle(..)
                            -- ** Specific variants of createProcess
                            , readProcess
                            , readProcessWithExitCode
                            -- * Process completion
                            , waitForProcess
                            , getProcessExitCode
                            , terminateProcess
                            , closeHandles
                            ) where

import           Prelude
import           Data.Maybe
import           Control.Monad
import           Control.Exception
                 
import           System.IO
import           System.Exit
import           System.Process (CmdSpec(..))
import qualified System.Process as P

-- | A cjail configuration
data CJailConf = CJailConf { cjUser    :: Maybe String
                           -- ^ User
                           , cjTimeout :: Maybe Int
                           -- ^ Timeout in seconds
                           , cjDir     :: FilePath
                           -- ^ Path to jail
                           }

instance Show CJailConf where
  show conf = unwords
               [ "cjail"
               , maybe ""  ("--user "++) (cjUser conf)
               , maybe ""  (("--timeout "++) . show) (cjTimeout conf)
               , cjDir conf
               , "" ]

-- | Data structure specifying how a command should be created
data CreateProcess = CreateProcess 
  { cmdspec :: CmdSpec
  -- ^ Executable and arguments, or BASH shell command
  , cwd     :: Maybe FilePath
  -- ^ Optional path to the working directory 
  , env     :: [(String, String)]
  -- ^ Environment for new process
  }

-- | Create a 'CreateProcess' representing a command to be passed to
-- the shell
shell :: String -> CreateProcess
shell cmd = CreateProcess { cmdspec = ShellCommand cmd
                          , cwd = Nothing
                          , env = [] }

-- | Create a 'CreateProcess' representing a raw command with arguements
proc :: FilePath -> [String] -> CreateProcess
proc cmd args = CreateProcess { cmdspec = RawCommand cmd args
                              , cwd = Nothing
                              , env = [] }

-- | Labeled handle to process
data ProcessHandle = ProcessHandle
  { stdIn  :: Handle
  -- ^ New standard in handle, will use default encoding and newline
  -- translation mode
  , stdOut :: Handle
  -- ^ New standard out handle, will use default encoding and newline
  -- translation mode
  , stdErr :: Handle
  -- ^ New standard error handle, will use default encoding and newline
  -- translation mode
  , processHandle :: P.ProcessHandle
  -- ^ Handle to process
  }

instance Show CmdSpec where
  show (ShellCommand s) = s
  show (RawCommand fp args) = unwords $ fp : args

-- | Create a process handle. The handle contains labeled
-- handles to standard in, standard out, and standard error. Moreover,
-- a handle to the process itself (to e.g., terminate it) is
-- also constructed. Internally, this function calls
-- 'System.Process.createProcess'.
--
-- For example to execute a simple @ls@ command:
--
-- > import qualified Data.ByteString.Lazy.Char8 as L8
-- > ...
-- >
-- > ls :: IO L8.ByteString
-- > ls = runCJail (CJailConf Nothing Nothing "/opt/myJail") $ do
-- >        lph <- createProcess (shell "ls")
-- >        liftLIO $ hGetContents $ stdOut lph
--
-- or write and read from (same) temporary file with @cat@:
--
-- > ex :: IO L8.ByteString
-- > ex = runCJail (CJailConf Nothing Nothing "/opt/myJail") $ do
-- >       lph <- createProcess (shell "cat > /tmp/xxx ; cat /tmp/xxx")
-- >       liftLIO $ hPutStrLn (stdIn lph) (L8.pack "hello jail")
-- >       liftLIO $ hClose (stdIn lph)
-- >       liftLIO $ hGetContents $ stdOut lph
--
-- Note that both of these examples use Lazy IO and thus the handles
-- are not closed. More appropriately, the result from the jailed
-- process should be forced (with e.g., 'evaluate') and the handles
-- should be closed. Consider for example sorting a list of numbers
-- with @sort@:
--
-- > sort :: [Int] -> IO [Int]
-- > sort ls = do
-- >   lph <- runCJail (CJailConf Nothing Nothing "/opt/myJail") $
-- >            createProcess (proc "sort" ["-n"])
-- >   let input = L8.pack . intercalate "\n" . map show $ ls
-- >   hPut (stdIn lph) input
-- >   hClose (stdIn lph)
-- >   bs <- whileNotEOF (stdOut lph) []
-- >   closeHandles lph
-- >   return bs
-- >     where whileNotEOF h acc = do
-- >             eof <- hIsEOF  h
-- >             if eof
-- >               then return acc
-- >               else do res <- (read . L8.unpack) `liftM` hGetLine h
-- >                       whileNotEOF h (res : acc)
--
--
createProcess :: CJailConf -> CreateProcess -> IO ProcessHandle
createProcess conf cp = do
  (mh0, mh1, mh2, ph) <- P.createProcess $ mkCreateProcess conf cp
  [h0, h1, h2] <- getHandlesOrError [mh0, mh1, mh2] ph
  return ProcessHandle { stdIn  = h0
                       , stdOut = h1
                       , stdErr = h2
                       , processHandle = ph }
    where getHandlesOrError mhs ph =
            let hs = catMaybes mhs
            in if length mhs == length hs
                 then return hs
                 else do mapM_ hClose hs
                         void $ P.terminateProcess ph
                         throwIO $ userError 
                            "createProcess could not create standard handles"

-- | Fork an external process and read it standard output strictly,
-- blocking until the process terminates and retuns an output string.
-- The function throws an 'IOError' if the exit code is not 'ExitSuccess'
-- Must compile with @-threaded@ if you want other threads to keep running
-- while blocking on the result of @readProcess@
readProcess :: CJailConf     -- ^ CJail configuration
            -> FilePath      -- ^ Executable
            -> [String]      -- ^ Arguments
            -> String        -- ^ Standard input
            -> IO String  -- ^ Standard output
readProcess conf exe exeArgs stdin' = do
  let (cjail, cjailArgs) = confToCmdArgs conf
      args = cjailArgs ++ (exe : exeArgs)
  P.readProcess cjail args stdin'

-- | Same as 'readProcess', but returns the exit code explicitly, and
-- strictly reads standard error.
readProcessWithExitCode
  :: CJailConf           -- ^ CJail configuration
  -> FilePath            -- ^ Executable
  -> [String]            -- ^ Arguments
  -> String              -- ^ Standard input
  -> IO (ExitCode, String, String)  -- ^ (exit code, stdout, stderr)
readProcessWithExitCode conf exe exeArgs stdin' = do
  let (cjail, cjailArgs) = confToCmdArgs conf
      args = cjailArgs ++ (exe : exeArgs)
  P.readProcessWithExitCode cjail args stdin'

--
-- Process completion
--

-- | Wait for specified process to terminate.  Must compile with
-- @-threaded@ if you want other threads to keep running while blocking
-- on the result.
waitForProcess :: ProcessHandle -> IO ExitCode
waitForProcess = P.waitForProcess . processHandle

-- | Get process exit code without blocking.
getProcessExitCode :: ProcessHandle -> IO (Maybe ExitCode)
getProcessExitCode = P.getProcessExitCode . processHandle

-- | Attempt to terminate the specified process.
-- As noted in "System.Process", this function should not be used under
-- normal circumstances. This function sends the process the @SIGTERM@
-- signal.
terminateProcess :: ProcessHandle -> IO ()
terminateProcess = P.terminateProcess .  processHandle
  

-- | Close all handles (simply calls 'hClose')
closeHandles :: ProcessHandle -> IO ()
closeHandles ph = mapM_ safeHClose [stdIn ph, stdOut ph, stdErr ph]
  where safeHClose h = hClose h `onException` return ()

--
-- Helpers
--

-- | Make a @CreateProcess@ value usable by "System.Process"
mkCreateProcess :: CJailConf -> CreateProcess -> P.CreateProcess
mkCreateProcess conf cp =
  let (cjail, cjailArgs) = confToCmdArgs conf
  in P.CreateProcess 
        { P.cmdspec      = RawCommand cjail $
                              cjailArgs ++ cmdSpecToArgs (cmdspec cp)
        , P.cwd          = cwd cp
        , P.env          = Just $ env cp
        , P.std_in       = P.CreatePipe
        , P.std_out      = P.CreatePipe
        , P.std_err      = P.CreatePipe
        , P.close_fds    = True
        , P.create_group = False }

-- | Conver 'CmdSpec' to list of arguments to @cjail@
cmdSpecToArgs :: CmdSpec -> [String]
cmdSpecToArgs (ShellCommand s) = ["bash", "-c", s]
cmdSpecToArgs (RawCommand fp s) = fp :s

-- | Convert configuration to pair of command and arguments.
confToCmdArgs :: CJailConf -> (String, [String])
confToCmdArgs conf = ("cjail", u ++ t ++ ["--", cjDir conf])
  where u = maybe [] (\x -> ["--user",x]) $ cjUser conf
        t = maybe [] (\x -> ["--timeout", show x ]) $ cjTimeout conf
