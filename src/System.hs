{-# LANGUAGE StrictData, DeriveGeneric #-}
module System where
import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import GameOverrides


-- | Type of each JSON entry in record syntax.
data System  =
  System { system :: !Text
         , command  :: !Text
         , overrides   :: [GameOverride]
           } deriving (Show,Generic)

-- Instances to convert our type to/from JSON.

instance FromJSON System
instance ToJSON System

