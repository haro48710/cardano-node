{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Node.Tracing.Peers
  ( NodePeers (..)
  , traceNodePeers
  ) where

import           Cardano.Prelude
import           Cardano.Logging
import           Data.Aeson (FromJSON, ToJSON)

import           Cardano.Node.Tracing.Tracers.Peer

type PeerInfoPP = Text -- The result of 'ppPeer' function.

-- | This type contains an information about current peers of the node.
--   It will be asked by external applications as a DataPoint.
newtype NodePeers blk = NodePeers [PeerInfoPP]

deriving instance Generic (NodePeers blk)

instance ToJSON (NodePeers blk)

-- Strictly speaking, we mustn't provide 'FromJSON' instance here,
-- but it will be convenient for acceptor application.
instance FromJSON (NodePeers blk)

-- | ...
traceNodePeers
  :: Trace IO (NodePeers blk)
  -> [PeerT blk]
  -> IO ()
traceNodePeers tr ev = traceWith tr $ NodePeers (map ppPeer ev)
