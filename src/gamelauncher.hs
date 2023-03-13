{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as B
import qualified Data.List as List
import Data.Text
import qualified Data.Text.Lazy as LazyText
import GHC.Generics
import qualified GameOverrides
import Options.Applicative
import qualified System as SystemData
import System.FilePath
import qualified System.Process as Process
import Text.Replace as Replace

data Options = Options
  { configFile :: FilePath,
    gameFile :: FilePath,
    system :: Text
  }
  deriving (Show)

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

expandCommand :: FilePath -> Text -> Text
expandCommand gamePath command =
  LazyText.toStrict $
    Replace.replaceWithList
      [ Replace.Replace "{file.path}" (pack gamePath),
        Replace.Replace "{file.name}" (pack $ takeFileName gamePath),
        Replace.Replace "{file.basename}" (pack $ takeBaseName gamePath),
        Replace.Replace "{file.dir}" (pack $ takeDirectory gamePath),
        Replace.Replace
          "{file.uri}"
          ( pack
              ( if isAbsolute gamePath
                  then "file://" ++ gamePath
                  else "file:/" ++ gamePath
              )
          )
      ]
      (LazyText.fromStrict command)

findCommand :: FilePath -> Text -> [SystemData.System] -> Maybe Text
findCommand gamePath systemName config =
  do
    system <- List.find matchingSystem config
    game <- List.find matchingGame (SystemData.overrides system)
    let command = GameOverrides.command game
    return (expandCommand gamePath command)
  where
    matchingSystem :: SystemData.System -> Bool
    matchingSystem c = SystemData.system c == systemName

    matchingGame :: GameOverrides.GameOverride -> Bool
    matchingGame g = GameOverrides.name g == pack (takeBaseName gamePath)

main :: IO ()
main = do
  -- Get JSON data and decode it
  opts <- execParser opts
  d <- J.eitherDecode <$> getJSON (configFile opts) :: IO (Either String [SystemData.System])
  -- If d is Left, the JSON was malformed.
  -- In that case, we report the error.
  -- Otherwise, we perform the operation of
  -- our choice. In this case, just print it.
  case d of
    Left err -> putStrLn err
    Right ps ->
      case findCommand (gameFile opts) (system opts) ps of
        Nothing -> return ()
        Just a -> Process.callCommand (unpack a)
  where
    opts =
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Command launcher based on game and system"
            <> header "GameLauncher"
        )
