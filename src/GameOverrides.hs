{-# LANGUAGE StrictData, DeriveGeneric #-}
module GameOverrides where
import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import GHC.Generics

data GameOverride =
  GameOverride {
    name :: !Text
  , command:: !Text
  } deriving (Show,Generic)

instance FromJSON GameOverride
instance ToJSON GameOverride
