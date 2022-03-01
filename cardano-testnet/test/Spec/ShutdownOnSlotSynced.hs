{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Spec.ShutdownOnSlotSynced
  ( hprop_shutdownOnSlotSynced
  ) where

import           Control.Monad
import           Data.Function
import           Data.Functor ((<&>))
import           Data.Int
import           Data.Maybe
import           Data.Ord
import           GHC.Num
import           Hedgehog (Property, assert, (===))
import           System.FilePath ((</>))
import           Text.Show (Show (..))

import           Data.Bool ((&&))
import           Data.List (filter, isInfixOf, last, lines, words)
import qualified Data.List as L
import           GHC.IO.Exception (ExitCode (ExitSuccess))
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Concurrent as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Directory as IO
import qualified System.IO as IO
import qualified System.Process as IO
import qualified Test.Base as H
import qualified Test.Process as H
import qualified Testnet.Conf as H
import           Text.Read (read)

{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant return" -}
{- HLINT ignore "Use let" -}

hprop_shutdownOnSlotSynced :: Property
hprop_shutdownOnSlotSynced = H.integration . H.runFinallies . H.workspace "chairman" $ \tempAbsBasePath' -> do
  projectBase <- H.note =<< H.noteIO . IO.canonicalizePath =<< H.getProjectBase
  H.Conf { H.tempBaseAbsPath, H.tempAbsPath, H.logDir, H.socketDir } <- H.noteShowM $ H.mkConf tempAbsBasePath' Nothing

  [port] <- H.noteShowIO $ IO.allocateRandomPorts 1

  H.createDirectoryIfMissing logDir

  sprocket <- H.noteShow $ IO.Sprocket tempBaseAbsPath (socketDir </> "node")

  H.diff (L.length (IO.sprocketArgumentName sprocket)) (<=) IO.maxSprocketArgumentNameLength

  H.createDirectoryIfMissing $ tempBaseAbsPath </> socketDir

  nodeStdoutFile <- H.noteTempFile logDir "node.stdout.log"
  nodeStderrFile <- H.noteTempFile logDir "node.stderr.log"

  hNodeStdout <- H.openFile nodeStdoutFile IO.WriteMode
  hNodeStderr <- H.openFile nodeStderrFile IO.WriteMode

  let maxSlot = 5000

  -- Run cardano-node with pipe as stdin.  Use 0 file descriptor as shutdown-ipc
  (_mStdin, _mStdout, _mStderr, pHandle, _releaseKey) <- H.createProcess =<<
    ( H.procNode
      [ "run"
      , "--config", projectBase </> "configuration/cardano/mainnet-config.json"
      , "--topology", projectBase </> "configuration/cardano/mainnet-topology.json"
      , "--database-path", tempAbsPath </> "db"
      , "--socket-path", IO.sprocketArgumentName sprocket
      , "--host-addr", "127.0.0.1"
      , "--port", show @Int port
      , "--shutdown-on-slot-synced", show maxSlot
      ] <&>
      ( \cp -> cp
        { IO.std_in = IO.CreatePipe
        , IO.std_out = IO.UseHandle hNodeStdout
        , IO.std_err = IO.UseHandle hNodeStderr
        , IO.cwd = Just tempBaseAbsPath
        }
      )
    )

  H.threadDelay $ 15 * 1000000

  mExitCodeRunning <- H.evalIO $ IO.getProcessExitCode pHandle

  when (isJust mExitCodeRunning) $ do
    H.evalIO $ IO.hClose hNodeStdout
    H.evalIO $ IO.hClose hNodeStderr
    H.cat nodeStdoutFile
    H.cat nodeStderrFile

  mExitCodeRunning === Just ExitSuccess

  log <- H.readFile nodeStdoutFile
  slotTip <- H.noteShow
              $ read @Integer
              $ last
              $ words
              $ last
              $ filter (isInfixOf "Closed db with immutable tip")
              $ lines log

  let epsilon = 5000
  assert (maxSlot <= slotTip && slotTip <= maxSlot + epsilon)

  return ()
