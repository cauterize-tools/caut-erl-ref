(name "erlang_test")
(version "0.0.1")
(fingerprint 43a81c33f4ace23fe0431dbd941203021de9195a)
(size 1 80)
(depth 6)
(typelength 2)
(lengthtag t1)
(type 
  unsigned8
  synonym
  (fingerprint e3e070dd78628d7aae3047878ccd6993423bb5bf)
  (size 1 1)
  (depth 2)
  u8)
(type 
  someenum
  enumeration
  (fingerprint e40274f6c896c769fa274bdfb53e6374428eb905)
  (size 1 1)
  (depth 1)
  t1
  (values (value red 0) (value green 1) (value blue 2)))
(type 
  some_range
  range
  (fingerprint d829f56fd4ee577222e9da4282f632581cee336d)
  (size 2 2)
  (depth 1)
  1000
  2010
  t2
  u16)
(type 
  primitivetest
  union
  (fingerprint 86837a082cb7501791d1ff52f200a2127ffce010)
  (size 2 9)
  (depth 2)
  t1
  (fields 
    (field u8 0 u8)
    (field u16 1 u16)
    (field u32 2 u32)
    (field u64 3 u64)
    (field s8 4 s8)
    (field s16 5 s16)
    (field s32 6 s32)
    (field s64 7 s64)
    (field bool 8 bool)
    (field f32 9 f32)
    (field f64 10 f64)))
(type 
  number64
  synonym
  (fingerprint a3db7c02b1f69e0897c4aeda37b2f5bc9b6aaa45)
  (size 8 8)
  (depth 2)
  s64)
(type 
  somearray
  array
  (fingerprint 1a9a2bf194ebd8e5287225bcbbabb4f592800ee5)
  (size 64 64)
  (depth 3)
  number64
  8)
(type 
  somevector
  vector
  (fingerprint 39b9e121f4c0d497424d602dbb955a29b0a6e1e3)
  (size 1 65)
  (depth 3)
  number64
  8
  t1)
(type 
  field_enum
  enumeration
  (fingerprint 49d934d221faf807f218f6a50a648e65be1acc8a)
  (size 1 1)
  (depth 1)
  t1
  (values (value somearray 0) (value somevector 1) (value arecord 2)))
(type 
  header
  vector
  (fingerprint a93f7513a7ddb2be400141d2b7bfe6cb4b8f369f)
  (size 1 5)
  (depth 2)
  field_enum
  4
  t1)
(type 
  crecord
  record
  (fingerprint bfd1b7a978b8202e3b73c600d6e3fb2da6fec5ad)
  (size 2 2)
  (depth 2)
  (fields (field a 0 s8) (field b 1 s8)))
(type 
  brecord
  record
  (fingerprint 16a32de194b2eb3cc2167cc5ff96738c61d1eeda)
  (size 3 3)
  (depth 3)
  (fields (field a 0 s8) (field d 1 crecord)))
(type 
  arecord
  record
  (fingerprint 9975e8a7ba429c1cd49ea366575b7c6c3e863cd8)
  (size 5 69)
  (depth 4)
  (fields (field z 0 somevector) (field a 1 s8) (field d 2 brecord)))
(type 
  a_union
  union
  (fingerprint d02d91ba842586a4e53f52892a4cd9a8794f1a00)
  (size 1 70)
  (depth 5)
  t1
  (fields 
    (field a 0 arecord)
    (field b 1 brecord)
    (field c 2 s8)
    (field d 3 number64)
    (empty e 4)))
(type 
  a_combination
  combination
  (fingerprint 71f00aeba696a2d73e26bcfdd6ebd9dac3a6cfba)
  (size 1 80)
  (depth 6)
  t1
  (fields (field a 0 number64) (field b 1 s8) (field c 2 a_union) (empty d 3)))
