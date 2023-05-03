{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as B
import qualified Data.List as List
import Data.Text hiding (elem, filter, head, map)
import qualified Data.Text.Lazy as LazyText
import GHC.Generics
import qualified GameOverrides
import Options.Applicative
import qualified System as SystemData
import System.Environment
import System.FilePath
import qualified System.Process as Process
import Text.Regex (matchRegex, mkRegex, subRegex)
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

filterEnvs :: [(String, String)] -> [String] -> [(String, String)]
filterEnvs environment selected = filter ((`elem` selected) . fst) environment

replaceEnvs :: String -> [(String, String)] -> String
replaceEnvs input [] = input
replaceEnvs input (firstEnv : rest) =
  let next = replaceEnvs input rest
   in subRegex (mkRegex "\\{env\\.(.*)}") next (snd firstEnv)

expandCommand :: FilePath -> Text -> [(String, String)] -> Text
expandCommand gamePath command environment =
  let formatFileURL path = if isAbsolute path then "file://" ++ path else "file:/" ++ path
      fileReplaced =
        unpack . LazyText.toStrict $
          Replace.replaceWithList
            [ Replace.Replace "{file.path}" (pack gamePath),
              Replace.Replace "{file.name}" (pack $ takeFileName gamePath),
              Replace.Replace "{file.basename}" (pack $ takeBaseName gamePath),
              Replace.Replace "{file.dir}" (pack $ takeDirectory gamePath),
              Replace.Replace "{file.uri}" (pack $ formatFileURL gamePath)
            ]
            (LazyText.fromStrict command)
      matchedEnvs = matchRegex (mkRegex "\\{env\\.(.*)}") fileReplaced
      envReplaced = case matchedEnvs of
        Just a -> replaceEnvs fileReplaced (filterEnvs environment a)
        Nothing -> fileReplaced
   in pack envReplaced

findCommand :: FilePath -> Text -> [SystemData.System] -> [(String, String)] -> Maybe Text
findCommand gamePath systemName config environment =
  do
    system <- List.find matchingSystem config
    game <- List.find matchingGame (SystemData.overrides system)
    let command = GameOverrides.command game
    return (expandCommand gamePath command environment)
  where
    matchingSystem :: SystemData.System -> Bool
    matchingSystem c = SystemData.system c == systemName

    matchingGame :: GameOverrides.GameOverride -> Bool
    matchingGame g = GameOverrides.name g == pack (takeBaseName gamePath)

main :: IO ()
main = do
  -- Get JSON data and decode it
  opts <- execParser opts
  environment <- getEnvironment
  d <- J.eitherDecode <$> getJSON (configFile opts) :: IO (Either String [SystemData.System])
  -- If d is Left, the JSON was malformed.
  -- In that case, we report the error.
  -- Otherwise, we perform the operation of
  -- our choice. In this case, just print it.
  case d of
    Left err -> putStrLn err
    Right ps ->
      case findCommand (gameFile opts) (system opts) ps environment of
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
