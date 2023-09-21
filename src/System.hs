{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module System where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B
import Data.Text
import GHC.Generics
import qualified GameOverrides

jsonOptions =
  defaultOptions
    { omitNothingFields = True
    }

-- | Type of each JSON entry in record syntax.
data System = System
  { system :: Text,
    emulator :: Maybe Text,
    profile :: Maybe Text,
    command :: Text,
    overrides :: [GameOverrides.GameOverride],
    precommands :: Maybe [Text],
    postcommands :: Maybe [Text]
  }
  deriving (Show, Generic)

-- Instances to convert our type to/from JSON.

instance FromJSON System

instance ToJSON System
