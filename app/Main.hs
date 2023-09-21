{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as B
import qualified Data.List as List
import qualified Data.List.NonEmpty
import qualified Data.Maybe
import Data.Text hiding (elem, filter, head, map, null)
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
    system :: Text,
    emulator :: Text,
    profile :: Text
  }
  deriving (Show)

configFileParser :: Parser FilePath
configFileParser = strOption (long "inputFile" <> short 'i' <> help "Input file " <> metavar "FILE")

gameFileParser :: Parser FilePath
gameFileParser = strOption (long "gameFile" <> short 'g' <> help "path to game file to launch" <> metavar "FILE")

systemParser :: Parser Text
systemParser = strOption (long "system" <> short 's' <> help "name of system" <> metavar "SYSTEM")

emulatorParser :: Parser Text
emulatorParser = strOption (long "emulator" <> short 'e' <> help "name of emulator" <> metavar "EMULATOR" <> value "")

profileParser :: Parser Text
profileParser = strOption (long "profile" <> short 'e' <> help "name of profile" <> metavar "PROFILE" <> value "")

optionsParser :: Parser Options
optionsParser = Options <$> configFileParser <*> gameFileParser <*> systemParser <*> emulatorParser <*> profileParser

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

wrapString :: Char -> String -> String
wrapString wrapper toWrap = [wrapper] ++ toWrap ++ [wrapper]

expandCommand :: FilePath -> [(String, String)] -> Text -> Text
expandCommand gamePath environment command =
  let formatFileURL path = if isAbsolute path then "file://" ++ path else "file:/" ++ path
      fileReplaced =
        unpack . LazyText.toStrict $
          Replace.replaceWithList
            [ Replace.Replace "{file.path}" (pack (wrapString '"' gamePath)),
              Replace.Replace "{file.name}" (pack $ (wrapString '"' $ takeFileName gamePath)),
              Replace.Replace "{file.basename}" (pack $ (wrapString '"' $ takeBaseName gamePath)),
              Replace.Replace "{file.dir}" (pack $ (wrapString '"' $ takeDirectory gamePath)),
              Replace.Replace "{file.uri}" (pack $ (wrapString '"' $ formatFileURL gamePath))
            ]
            (LazyText.fromStrict command)
      matchedEnvs = matchRegex (mkRegex "\\{env\\.(.*)}") fileReplaced
      envReplaced = case matchedEnvs of
        Just a -> replaceEnvs fileReplaced (filterEnvs environment a)
        Nothing -> fileReplaced
   in pack envReplaced

reduceUntilEmpty :: [a -> Bool] -> Data.List.NonEmpty.NonEmpty a -> a
reduceUntilEmpty [] as = Data.List.NonEmpty.head as
reduceUntilEmpty (p : ps) as = case Data.List.NonEmpty.nonEmpty $ Data.List.NonEmpty.filter p as of
  Nothing -> Data.List.NonEmpty.head as
  Just result -> reduceUntilEmpty ps result

findCommands :: FilePath -> Text -> Text -> Text -> [SystemData.System] -> [(String, String)] -> Maybe [Text]
findCommands gamePath systemName emulatorName profileName config environment =
  do
    systems <- Data.List.NonEmpty.nonEmpty $ List.filter matchingSystem config
    let selection = reduceUntilEmpty [matchingSystem, matchingEmulator, matchingProfile] systems
    let game = List.find matchingGame (SystemData.overrides selection)
    let command =
          case game of
            Just g -> GameOverrides.command g
            Nothing -> SystemData.command selection
    let commands = Data.Maybe.fromMaybe [] (SystemData.precommands selection) <> [command] <> Data.Maybe.fromMaybe [] (SystemData.postcommands selection)
    return $ fmap (expandCommand gamePath environment) commands
  where
    matchingSystem :: SystemData.System -> Bool
    matchingSystem c = SystemData.system c == systemName
    matchingEmulator :: SystemData.System -> Bool
    matchingEmulator c = SystemData.emulator c == Just emulatorName
    matchingProfile :: SystemData.System -> Bool
    matchingProfile c = SystemData.profile c == Just profileName

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
      case findCommands (gameFile opts) (system opts) (emulator opts) (profile opts) ps environment of
        Nothing -> return ()
        Just a -> do
          mapM_ (Process.callCommand . unpack) a
  where
    opts =
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Command launcher based on game and system"
            <> header "GameLauncher"
        )
