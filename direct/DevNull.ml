open Dessser

module Ser (BE : BACKEND) : SER with module BE = BE =
struct
  module BE = BE

  type state = unit
  let start _typ _oc p = (), p
  let stop _oc () p = p

  type 'a ser = BE.output -> frame list -> state -> 'a -> [`Pointer] id -> [`Pointer] id

  let sfloat oc _frames () v p = BE.ignore oc v ; p
  let sstring oc _frames () v p = BE.ignore oc v ; p
  let sbool oc _frames () v p = BE.ignore oc v ; p
  let si8 oc _frames () v p = BE.ignore oc v ; p
  let su8 oc _frames () v p = BE.ignore oc v ; p
  let si16 oc _frames () v p = BE.ignore oc v ; p
  let su16 oc _frames () v p = BE.ignore oc v ; p
  let si32 oc _frames () v p = BE.ignore oc v ; p
  let su32 oc _frames () v p = BE.ignore oc v ; p
  let si64 oc _frames () v p = BE.ignore oc v ; p
  let su64 oc _frames () v p = BE.ignore oc v ; p
  let si128 oc _frames () v p = BE.ignore oc v ; p
  let su128 oc _frames () v p = BE.ignore oc v ; p
  let tup_opn _ _frames () p = p
  let tup_cls _ _frames () p = p
  let tup_sep _ _ _frames () p = p
  let rec_opn _ _frames () p = p
  let rec_cls _ _frames () p = p
  let rec_sep _ _ _frames () p = p
  let vec_opn _ _frames () p = p
  let vec_cls _ _frames () p = p
  let vec_sep _ _ _frames () p = p
  let nullable _ _frames () p = p
  let snull _ _frames () p = p
  let snotnull _ _frames () p = p

  type 'a ssizer = BE.output -> frame list -> 'a -> ssize
  let ssize_of_float _ _ _ = ConstSize 0
  let ssize_of_string _ _ _ = ConstSize 0
  let ssize_of_bool _ _ _ = ConstSize 0
  let ssize_of_i8 _ _ _ = ConstSize 0
  let ssize_of_i16 _ _ _ = ConstSize 0
  let ssize_of_i32 _ _ _ = ConstSize 0
  let ssize_of_i64 _ _ _ = ConstSize 0
  let ssize_of_i128 _ _ _ = ConstSize 0
  let ssize_of_u8 _ _ _ = ConstSize 0
  let ssize_of_u16 _ _ _ = ConstSize 0
  let ssize_of_u32 _ _ _ = ConstSize 0
  let ssize_of_u64 _ _ _ = ConstSize 0
  let ssize_of_u128 _ _ _ = ConstSize 0
  let ssize_of_tup _ _ _ = ConstSize 0
  let ssize_of_rec _ _ _ = ConstSize 0
  let ssize_of_vec _ _ _ = ConstSize 0
  let ssize_of_null _ _ = ConstSize 0
end
