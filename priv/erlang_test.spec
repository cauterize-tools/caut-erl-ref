(name "erlang_test")
(version "0.0.1")
(fingerprint 6e7bb1081105639bda93107275e0459c2dd8b2c2)
(size 1 80)
(depth 6)
(typelength 1)
(lengthtag t1)
(type 
  unsigned8
  synonym
  (fingerprint e3e070dd78628d7aae3047878ccd6993423bb5bf)
  (size 1 1)
  (depth 2)
  u8)
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