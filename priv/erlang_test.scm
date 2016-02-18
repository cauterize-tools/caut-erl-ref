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
(type someenum enumeration
      (values
        red
        green
        blue))
(type primitivetest union
      (fields
        (field u8 u8)
        (field u16 u16)
        (field u32 u32)
        (field u64 u64)
        (field s8 s8)
        (field s16 s16)
        (field s32 s32)
        (field s64 s64)
        (field bool bool)
        (field f32 f32)
        (field f64 f64)))
(type some_range range 1000 1010)
(type field_enum enumeration
      (values
        somearray
        somevector
        arecord))
(type header vector field_enum 4)
