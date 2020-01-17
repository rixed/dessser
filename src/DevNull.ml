open Dessser
open DessserTypes
open DessserExpressions

module Ser : SER =
struct
  type state = unit
  let ptr _vtyp = dataptr

  let start _vtyp p = (), p
  let stop () p = p

  type ser = state -> e -> e -> e

  let sfloat () _v p = p
  let sstring () _v p = p
  let sbool () _v p = p
  let si8 () _v p = p
  let su8 () _v p = p
  let si16 () _v p = p
  let su16 () _v p = p
  let si24 () _v p = p
  let su24 () _v p = p
  let si32 () _v p = p
  let su32 () _v p = p
  let si40 () _v p = p
  let su40 () _v p = p
  let si48 () _v p = p
  let su48 () _v p = p
  let si56 () _v p = p
  let su56 () _v p = p
  let si64 () _v p = p
  let su64 () _v p = p
  let si128 () _v p = p
  let su128 () _v p = p
  let schar () _v p = p
  let tup_opn () _ p = p
  let tup_cls () p = p
  let tup_sep _ () p = p
  let rec_opn () _ p = p
  let rec_cls () p = p
  let rec_sep _ () p = p
  let vec_opn () _ _ p = p
  let vec_cls () p = p
  let vec_sep _ () p = p
  let list_opn () _ _ p = p
  let list_cls () p = p
  let list_sep () p = p
  let nullable () p = p
  let snull _t () p = p
  let snotnull _t () p = p

  type ssizer = maybe_nullable -> path -> e -> ssize
  let ssize_of_float _ _ _ = ConstSize 0
  let ssize_of_string _ _ _ = ConstSize 0
  let ssize_of_bool _ _ _ = ConstSize 0
  let ssize_of_char _ _ _ = ConstSize 0
  let ssize_of_i8 _ _ _ = ConstSize 0
  let ssize_of_i16 _ _ _ = ConstSize 0
  let ssize_of_i24 _ _ _ = ConstSize 0
  let ssize_of_i32 _ _ _ = ConstSize 0
  let ssize_of_i40 _ _ _ = ConstSize 0
  let ssize_of_i48 _ _ _ = ConstSize 0
  let ssize_of_i56 _ _ _ = ConstSize 0
  let ssize_of_i64 _ _ _ = ConstSize 0
  let ssize_of_i128 _ _ _ = ConstSize 0
  let ssize_of_u8 _ _ _ = ConstSize 0
  let ssize_of_u16 _ _ _ = ConstSize 0
  let ssize_of_u24 _ _ _ = ConstSize 0
  let ssize_of_u32 _ _ _ = ConstSize 0
  let ssize_of_u40 _ _ _ = ConstSize 0
  let ssize_of_u48 _ _ _ = ConstSize 0
  let ssize_of_u56 _ _ _ = ConstSize 0
  let ssize_of_u64 _ _ _ = ConstSize 0
  let ssize_of_u128 _ _ _ = ConstSize 0
  let ssize_of_tup _ _ _ = ConstSize 0
  let ssize_of_rec _ _ _ = ConstSize 0
  let ssize_of_vec _ _ _ = ConstSize 0
  let ssize_of_list _ _ _ = ConstSize 0
  let ssize_of_null _ _ = ConstSize 0
end
