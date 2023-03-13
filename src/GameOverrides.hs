{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module GameOverrides where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Text
import GHC.Generics

data GameOverride = GameOverride
  { name :: !Text,
    command :: !Text
  }
  deriving (Show, Generic)

instance FromJSON GameOverride

instance ToJSON GameOverride
