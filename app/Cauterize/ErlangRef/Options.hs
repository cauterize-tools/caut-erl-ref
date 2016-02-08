module Cauterize.ErlangRef.Options
  ( runWithOptions
  , ErlangOpts(..)
  ) where

import Options.Applicative

runWithOptions :: (ErlangOpts -> IO ()) -> IO ()
runWithOptions fn = execParser options >>= fn

data ErlangOpts = ErlangOpts
  { specFile :: FilePath
  , outputDirectory :: FilePath
  } deriving (Show)

options :: ParserInfo ErlangOpts
options = info (optParser <**> helper)
            ( fullDesc
           <> progDesc "Process Cauterize schema files."
            )

optParser :: Parser ErlangOpts
optParser = ErlangOpts
  <$> strOption
    ( long "spec"
   <> short 's'
   <> metavar "FILE_PATH"
   <> help "Input Cauterize specification file."
    )
  <*> strOption
    ( long "output"
   <> short 'o'
   <> metavar "DIRECTORY_PATH"
   <> help "Output Cauterize directory."
    )
