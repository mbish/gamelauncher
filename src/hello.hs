{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import System.FilePath
import qualified Data.Aeson as J
import Data.Text
import qualified Data.List as List
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import qualified System as SystemData
import qualified GameOverrides as GameOverrides
import Options.Applicative

data Options = Options {
    configFile :: FilePath
  , gameFile :: FilePath
  , system :: Text
} deriving(Show)

configFileParser :: Parser FilePath
configFileParser = strOption (long "inputFile" <> short 'i' <> help "Input file " <> metavar "FILE")

gameFileParser :: Parser FilePath
gameFileParser = strOption (long "gameFile" <> short 'g' <> help "path to game file to launch" <> metavar "FILE")

systemParser :: Parser Text
systemParser = strOption (long "system" <> short 's' <> help "name of system" <> metavar "SYSTEM")

optionsParser :: Parser Options
optionsParser = Options <$> configFileParser <*> gameFileParser <*> systemParser

-- Read the local copy of the JSON file.
getJSON :: FilePath -> IO B.ByteString
getJSON = B.readFile

findCommand :: FilePath -> Text -> [SystemData.System] -> Maybe Text
findCommand game systemName config =
  List.find matchingSystem config >>= (\system ->
    case List.find matchingGame (SystemData.overrides system) of
        Nothing -> return (SystemData.command system)
        Just g -> return (GameOverrides.command g)
  )
  where
    matchingSystem :: SystemData.System -> Bool
    matchingSystem c = (SystemData.system c) == systemName

    matchingGame :: GameOverrides.GameOverride -> Bool
    matchingGame g = (GameOverrides.name g) == pack (takeBaseName game)

main :: IO ()
main = do
 -- Get JSON data and decode it
 opts <- execParser opts
 d <- (J.eitherDecode <$> (getJSON (configFile opts))) :: IO (Either String [SystemData.System])
 -- If d is Left, the JSON was malformed.
 -- In that case, we report the error.
 -- Otherwise, we perform the operation of
 -- our choice. In this case, just print it.
 case d of
  Left err -> putStrLn err
  Right ps -> print $
    case findCommand (gameFile opts) (system opts) ps of
        Nothing -> Nothing
        Just a -> Just a
 where
    opts = info (optionsParser <**> helper)  ( fullDesc
     <> progDesc "Command launcher based on game and system"
     <> header "GameLauncher" )
