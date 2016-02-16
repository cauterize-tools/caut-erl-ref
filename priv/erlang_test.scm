(name "erlang_test")
(version "0.0.1")
(type number64 synonym s64)
(type unsigned8 synonym u8)
(type somearray array number64 8)
(type somevector vector number64 8)
(type arecord record
        (fields
          (field z somevector)
          (field a s8)
          (field d brecord)))
(type brecord record
        (fields
          (field a s8)
          (field d crecord)))
(type crecord record
        (fields
          (field a s8)
          (field b s8)))
(type a_union union
       (fields
         (field a arecord)
         (field b brecord)
         (field c s8)
         (field d number64)
         (empty e)))
(type a_combination combination
             (fields
               (field a number64)
               (field b s8)
               (field c a_union)
               (empty d)))
