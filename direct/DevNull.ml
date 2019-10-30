open Dessser

module Ser (BE : BACKEND) : SER with module BE = BE =
struct
  module BE = BE

  type state = unit
  let init_state _typ _oc p = (), p

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
end
