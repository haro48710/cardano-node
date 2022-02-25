{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Node.Tracing.StateRep
  ( ChainDBTraceEvent (..)
  , NodeState (..)
  , traceNodeStateChainDB
  , traceNodeStateStartup
  , traceNodeStateShutdown
  ) where

import           Cardano.Prelude
import           Cardano.Logging
import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map

import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB

import           Cardano.Node.Handlers.Shutdown (ShutdownTrace (..))
import qualified Cardano.Node.Startup as Startup
import           Cardano.Node.TraceConstraints

data ChainDBTraceEvent
  = TraceAddBlockEvent          Text
  | TraceFollowerEvent          Text
  | TraceCopyToImmutableDBEvent Text
  | TraceGCEvent                Text
  | TraceInitChainSelEvent      Text
  | TraceOpenEvent              Text
  | TraceIteratorEvent          Text
  | TraceLedgerEvent            Text
  | TraceLedgerReplayEvent      Text
  | TraceImmutableDBEvent       Text
  | TraceVolatileDBEvent        Text

data StartupTraceEvent
  = StartupInfo
      [Text]
      Text
      [(Text, Text)]
      [(Text, Text)]
  | StartupP2PInfo           Text
  | StartupTime              Text
  | StartupNetworkMagic      Text
  | StartupSocketConfigError Text
  | StartupDBValidation
  | NetworkConfigUpdate
  | NetworkConfigUpdateError Text
  | NetworkConfig
      [(Int, Text)]
      [Text]
      Text
  | P2PWarning
  | P2PWarningDevelopementNetworkProtocols
  | WarningDevelopmentNetworkProtocols [Text] [Text]
  | BICommon  Text
  | BIShelley Text
  | BIByron   Text
  | BINetwork Text

-- | The representation of the current state of node.
--   All node states prior to tracing system going online are effectively invisible.
data NodeState blk
  = NodeTracingOnlineConfiguring
  | NodeStartup StartupTraceEvent
  | NodeChainDBState ChainDBTraceEvent
  | NodeShutdown ShutdownTrace

deriving instance Generic ChainDBTraceEvent
deriving instance Generic StartupTraceEvent
deriving instance Generic (NodeState blk)

instance ToJSON ChainDBTraceEvent
instance ToJSON StartupTraceEvent
instance ToJSON (NodeState blk)

-- | Strictly speaking, we mustn't provide 'FromJSON' instance here,
--   but it will be convenient for acceptor application.
instance FromJSON ChainDBTraceEvent
instance FromJSON StartupTraceEvent
instance FromJSON (NodeState blk)

-- | ...
traceNodeStateChainDB
  :: forall blk.
     ( TraceConstraints blk
     , LedgerSupportsProtocol blk
     , InspectLedger blk
     )
  => Trace IO (NodeState blk)
  -> ChainDB.TraceEvent blk
  -> IO ()
traceNodeStateChainDB tr ev =
  case ev of
    ChainDB.TraceAddBlockEvent ev' ->
      traceWith tr $ NodeChainDBState $ TraceAddBlockEvent (show ev')
    ChainDB.TraceFollowerEvent ev' ->
      traceWith tr $ NodeChainDBState $ TraceFollowerEvent (show ev')
    ChainDB.TraceCopyToImmutableDBEvent ev' ->
      traceWith tr $ NodeChainDBState $ TraceCopyToImmutableDBEvent (show ev')
    ChainDB.TraceGCEvent ev' ->
      traceWith tr $ NodeChainDBState $ TraceGCEvent (show ev')
    ChainDB.TraceInitChainSelEvent ev' ->
      traceWith tr $ NodeChainDBState $ TraceInitChainSelEvent (show ev')
    ChainDB.TraceOpenEvent ev' ->
      traceWith tr $ NodeChainDBState $ TraceOpenEvent (show ev')
    ChainDB.TraceIteratorEvent ev' ->
      traceWith tr $ NodeChainDBState $ TraceIteratorEvent (show ev')
    ChainDB.TraceLedgerEvent ev' ->
      traceWith tr $ NodeChainDBState $ TraceLedgerEvent (show ev')
    ChainDB.TraceLedgerReplayEvent ev' ->
      traceWith tr $ NodeChainDBState $ TraceLedgerReplayEvent (show ev')
    ChainDB.TraceImmutableDBEvent ev' ->
      traceWith tr $ NodeChainDBState $ TraceImmutableDBEvent (show ev')
    ChainDB.TraceVolatileDBEvent ev' ->
      traceWith tr $ NodeChainDBState $ TraceVolatileDBEvent (show ev')

traceNodeStateStartup
  :: Trace IO (NodeState blk)
  -> Startup.StartupTrace blk
  -> IO ()
traceNodeStateStartup tr ev =
  case ev of
    Startup.StartupInfo s l n2n n2c ->
      traceWith tr $ NodeStartup $ StartupInfo
                                     (map show s)
                                     (show l)
                                     [(show x, "TODO") | (x, _y) <- Map.toList n2n]
                                     [(show x, "TODO") | (x, _y) <- Map.toList n2c]
    Startup.StartupP2PInfo i ->
      traceWith tr $ NodeStartup $ StartupP2PInfo (show i)
    Startup.StartupTime t ->
      traceWith tr $ NodeStartup $ StartupTime (show t)
    Startup.StartupNetworkMagic nm ->
      traceWith tr $ NodeStartup $ StartupNetworkMagic (show nm)
    Startup.StartupSocketConfigError e ->
      traceWith tr $ NodeStartup $ StartupSocketConfigError (show e)
    Startup.StartupDBValidation ->
      traceWith tr $ NodeStartup StartupDBValidation
    Startup.NetworkConfigUpdate ->
      traceWith tr $ NodeStartup NetworkConfigUpdate
    Startup.NetworkConfigUpdateError t ->
      traceWith tr $ NodeStartup $ NetworkConfigUpdateError t
    Startup.NetworkConfig aps raps ula ->
      traceWith tr $ NodeStartup $ NetworkConfig
                                     [(i, show rap) | (i, rap) <- aps]
                                     (map show raps)
                                     (show ula)
    Startup.P2PWarning ->
      traceWith tr $ NodeStartup P2PWarning
    Startup.P2PWarningDevelopementNetworkProtocols ->
      traceWith tr $ NodeStartup P2PWarningDevelopementNetworkProtocols
    Startup.WarningDevelopmentNetworkProtocols n2n n2c ->
      traceWith tr $ NodeStartup $ WarningDevelopmentNetworkProtocols
                                     (map show n2n)
                                     (map show n2c)
    Startup.BICommon bi ->
      traceWith tr $ NodeStartup $ BICommon (show bi)
    Startup.BIShelley bi ->
      traceWith tr $ NodeStartup $ BIShelley (show bi)
    Startup.BIByron bi ->
      traceWith tr $ NodeStartup $ BIByron (show bi)
    Startup.BINetwork bi ->
      traceWith tr $ NodeStartup $ BINetwork (show bi)

traceNodeStateShutdown
  :: Trace IO (NodeState blk)
  -> ShutdownTrace
  -> IO ()
traceNodeStateShutdown tr ev =
  traceWith tr $ NodeShutdown ev
