{-# LANGUAGE QuasiQuotes #-}
module Cauterize.ErlangRef.Generate
       ( erlFileFromSpec
       ) where

import qualified Cauterize.Specification as S

import Data.Text (unpack)
import Data.String.Interpolate
import Data.String.Interpolate.Util

erlFileFromSpec :: S.Specification -> String
erlFileFromSpec s = unindent [i|
%% #{ln}
|]
  where
    ln = unpack (S.specName s)
