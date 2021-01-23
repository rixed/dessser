open Dessser
module T = DessserTypes
module E = DessserExpressions

module Ser : SER with type config = unit =
struct
  type config = unit
  type state = unit
  let ptr _vtyp = T.dataptr

  let start ?(config=()) _vtyp p = config, p
  let stop () p = p

  type ser = state -> T.maybe_nullable -> T.path -> E.t -> E.t -> E.t

  let sfloat () _ _ _v p = p
  let sstring () _ _ _v p = p
  let sbool () _ _ _v p = p
  let si8 () _ _ _v p = p
  let su8 () _ _ _v p = p
  let si16 () _ _ _v p = p
  let su16 () _ _ _v p = p
  let si24 () _ _ _v p = p
  let su24 () _ _ _v p = p
  let si32 () _ _ _v p = p
  let su32 () _ _ _v p = p
  let si40 () _ _ _v p = p
  let su40 () _ _ _v p = p
  let si48 () _ _ _v p = p
  let su48 () _ _ _v p = p
  let si56 () _ _ _v p = p
  let su56 () _ _ _v p = p
  let si64 () _ _ _v p = p
  let su64 () _ _ _v p = p
  let si128 () _ _ _v p = p
  let su128 () _ _ _v p = p
  let schar () _ _ _v p = p
  let tup_opn () _ _ _ p = p
  let tup_cls () _ _ p = p
  let tup_sep () _ _ p = p
  let rec_opn () _ _ _ p = p
  let rec_cls () _ _ p = p
  let rec_sep () _ _ p = p
  let sum_opn () _ _ _ _ p = p
  let sum_cls () _ _ p = p
  let vec_opn () _ _ _ _ p = p
  let vec_cls () _ _ p = p
  let vec_sep () _ _ p = p
  let list_opn () _ _ _ _ p = p
  let list_cls () _ _ p = p
  let list_sep () _ _ p = p
  let nullable () _ _ p = p
  let snull _t () _ _ p = p
  let snotnull _t () _ _ p = p

  type ssizer = T.maybe_nullable -> T.path -> E.t -> ssize
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
  let ssize_of_sum _ _ _ = ConstSize 0
  let ssize_of_vec _ _ _ = ConstSize 0
  let ssize_of_list _ _ _ = ConstSize 0
  let ssize_of_null _ _ = ConstSize 0
end