{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module System where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Text
import GHC.Generics
import GameOverrides

-- | Type of each JSON entry in record syntax.
data System = System
  { system :: !Text,
    command :: !Text,
    overrides :: [GameOverride]
  }
  deriving (Show, Generic)

-- Instances to convert our type to/from JSON.

instance FromJSON System

instance ToJSON System
