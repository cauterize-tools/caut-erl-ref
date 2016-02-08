module Main where

import Cauterize.ErlangRef.Options
import Cauterize.ErlangRef.Generate
import System.Directory
import System.FilePath.Posix
import Data.Text (unpack)

import qualified Cauterize.Specification as S

main :: IO ()
main = runWithOptions caut2erlang

caut2erlang :: ErlangOpts -> IO ()
caut2erlang ErlangOpts { specFile = sf, outputDirectory = od } = createGuard od $ do
  spec <- loadSpec sf
  let baseName = unpack $ S.specName spec
  generateDynamicFiles od baseName spec
  where
    loadSpec :: FilePath -> IO S.Specification
    loadSpec p = do
      s <- S.parseSpecificationFromFile p
      case s of
        Left e -> error $ show e
        Right s' -> return s'

createGuard :: FilePath -> IO a -> IO a
createGuard out go = do
  fe <- doesFileExist out
  de <- doesDirectoryExist out

  if fe
    then error $ "Error: " ++ out ++ " is a file."
    else if de
          then go
          else createDirectory out >> go

generateDynamicFiles :: FilePath -> String -> S.Specification -> IO ()
generateDynamicFiles path baseName spec = do
  writeFile (path `combine` (baseName ++ ".erl")) (erlFileFromSpec spec)
