{-# LANGUAGE QuasiQuotes #-}
module Cauterize.ErlangRef.Generate
       ( erlFileFromSpec
       ) where

import qualified Cauterize.CommonTypes as C
import qualified Cauterize.Specification as S
import qualified Cauterize.Hash as H

import Data.List (intercalate)
import Data.Text (unpack)
import Data.String.Interpolate
import Data.String.Interpolate.Util
import Numeric (showHex)

erlFileFromSpec :: S.Specification -> String
erlFileFromSpec s = unindent [i|
-module(#{ln}).
-export([decode/2, encode/1, fingerprints/0, tag_size/0, max_size/0]).

%% specification for Cauterize schema #{ln}

-define(CAUT_SPEC_#{ln}, [
#{descriptorList}
]).

-define(CAUT_SPEC_#{ln}_tag_size, #{sts}).
-define(CAUT_SPEC_#{ln}_max_size, #{sms}).
-define(CAUT_SPEC_#{ln}_fp_lookup, [
#{typeFps}
]).

-spec decode(Bin :: binary(), Which :: atom()) -> cauterize:decode_result().
decode(Bin, Which) ->
  cauterize:decode(Bin, Which, ?CAUT_SPEC_#{ln}).

-spec encode([{TypeName :: atom(), Value :: any()}, ...]) -> cauterize:encode_result().
encode(Inst) ->
  cauterize:encode(Inst, ?CAUT_SPEC_#{ln}).

-spec fingerprints() -> [{Type :: atom(), Fingerprint :: binary()},...].
fingerprints() ->
  ?CAUT_SPEC_#{ln}_fp_lookup.

-spec tag_size() -> pos_integer().
tag_size() ->
  ?CAUT_SPEC_#{ln}_tag_size.

-spec max_size() -> pos_integer().
max_size() ->
  ?CAUT_SPEC_#{ln}_max_size.
|]
  where
    ln = unpack (S.specName s)
    types = S.specTypes s
    sts = S.specTypeLength s
    sms = C.sizeMax . S.specSize $ s
    descriptorList = intercalate ",\n" $ map descriptor types
    typeFps = intercalate ",\n" $ map fingerprint types
    fingerprint t =
      let n = ident2str ident
          ident = S.typeName t
          fp = formatFp (S.typeFingerprint t)
      in [i|  {#{n}, <<#{fp}>>}|]

descriptor t =
  let n = ident2str ident
      ident = S.typeName t
      tps = typeToPrimString t
      typeDesc = typeDescToString (S.typeDesc t)
  in [i|  {descriptor, #{tps}, #{n}, #{typeDesc}}|]


formatFp :: H.Hash -> String
formatFp f =
  let bs = H.hashToBytes f
      showByte n = case showHex n "" of
                     [a] -> ['1', '6', '#', '0', a]
                     [a,b] -> ['1', '6', '#', a, b]
                     _ -> error "formatFp: should be impossible"
  in intercalate "," (map showByte bs)

ident2str :: C.Identifier -> String
ident2str = unpack . C.unIdentifier

typeToPrimString :: S.Type -> String
typeToPrimString S.Type { S.typeDesc = d } = n
  where
    n = case d of
      S.Synonym {}     -> "synonym"
      S.Range {}       -> "range"
      S.Array {}       -> "array"
      S.Vector {}      -> "vector"
      S.Enumeration {} -> "enumeration"
      S.Record {}      -> "record"
      S.Combination {} -> "combination"
      S.Union {}       -> "union"

tag2str :: C.Tag -> String
tag2str C.T1 = "tag8";
tag2str C.T2 = "tag16";
tag2str C.T4 = "tag32";
tag2str C.T8 = "tag64";

prim2str :: C.Prim -> String
prim2str C.PU8   = "u8"
prim2str C.PU16  = "u16"
prim2str C.PU32  = "u32"
prim2str C.PU64  = "u64"
prim2str C.PS8   = "s8"
prim2str C.PS16  = "s16"
prim2str C.PS32  = "s32"
prim2str C.PS64  = "s64"
prim2str C.PF32  = "f32"
prim2str C.PF64  = "f64"
prim2str C.PBool = "bool"

typeDescToString :: S.TypeDesc -> String
typeDescToString d =
  case d of
    S.Synonym { S.synonymRef = r }
      -> [i|#{ident2str r}|]
    S.Range { S.rangeOffset = ro, S.rangeLength = rl, S.rangeTag = rt }
      -> [i|{ #{ro}, #{rl}, #{tag2str rt} }|]
    S.Array { S.arrayRef = r, S.arrayLength = al }
      -> [i|{ #{ident2str r}, #{al} }|]
    S.Vector { S.vectorRef = r, S.vectorLength = vl, S.vectorTag = vt }
      -> [i|{ #{ident2str r}, #{vl}, #{tag2str vt} }|]
    S.Enumeration { S.enumerationTag = et, S.enumerationValues = evs }
      -> let evsStr = intercalate ", " $ map e2tup evs
             e2tup (S.EnumVal v ix) = [i|{#{ident2str v}, #{ix}}|]
         in [i|{ #{tag2str et}, [#{evsStr}] }|]
    S.Record { S.recordFields = rs }
      -> let fieldsStr = intercalate ", " $ map f2str rs
         in [i|[#{fieldsStr}]|]
    S.Combination { S.combinationFields = cf, S.combinationTag = ct }
      -> let fieldsStr = intercalate ", " $ map f2str cf
         in [i|{ #{tag2str ct}, [#{fieldsStr}] }|]
    S.Union { S.unionFields = uf, S.unionTag = ut }
      -> let fieldsStr = intercalate ", " $ map f2str uf
         in [i|{ #{tag2str ut}, [#{fieldsStr}] }|]

f2str :: S.Field -> String
f2str S.EmptyField { S.fieldName = n, S.fieldIndex = ix } =
  [i|{ empty, #{ident2str n}, #{ix}}|]
f2str S.DataField { S.fieldName = n, S.fieldIndex = ix, S.fieldRef = r } =
  [i|{ data, #{ident2str n}, #{ix}, #{ident2str r}}|]
