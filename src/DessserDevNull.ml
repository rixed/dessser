open Dessser
open DessserMiscTypes
module T = DessserTypes
module E = DessserExpressions
module Path = DessserPath
open E.Ops

module Ser : SER with type config = unit =
struct
  let id = Null

  type config = unit
  type state = unit
  let ptr _vtyp = T.ptr

  let start ?(config=()) _vtyp _l p = config, p
  let stop () _l p = p

  type ser = state -> T.mn -> Path.t -> E.env -> E.t -> E.t -> E.t

  let sfloat () _ _ _l _v p = p
  let sstring () _ _ _l _v p = p
  let sbool () _ _ _l _v p = p
  let si8 () _ _ _l _v p = p
  let su8 () _ _ _l _v p = p
  let si16 () _ _ _l _v p = p
  let su16 () _ _ _l _v p = p
  let si24 () _ _ _l _v p = p
  let su24 () _ _ _l _v p = p
  let si32 () _ _ _l _v p = p
  let su32 () _ _ _l _v p = p
  let si40 () _ _ _l _v p = p
  let su40 () _ _ _l _v p = p
  let si48 () _ _ _l _v p = p
  let su48 () _ _ _l _v p = p
  let si56 () _ _ _l _v p = p
  let su56 () _ _ _l _v p = p
  let si64 () _ _ _l _v p = p
  let su64 () _ _ _l _v p = p
  let si128 () _ _ _l _v p = p
  let su128 () _ _ _l _v p = p
  let schar () _ _ _l _v p = p
  let tup_opn () _ _ _ _ p = p
  let tup_cls () _ _ _ p = p
  let tup_sep () _ _ _ p = p
  let rec_opn () _ _ _ _ p = p
  let rec_cls () _ _ _ p = p
  let rec_sep () _ _ _ p = p
  let sum_opn () _ _ _ _ _ p = p
  let sum_cls () _ _ _ p = p
  let vec_opn () _ _ _ _ _ p = p
  let vec_cls () _ _ _ p = p
  let vec_sep () _ _ _ p = p
  let list_opn () _ _ _ _ _ p = p
  let list_cls () _ _ _ p = p
  let list_sep () _ _ _ p = p
  let nullable () _ _ _ p = p
  let snull _t () _ _ _ p = p
  let snotnull _t () _ _ _ p = p

  type ssizer = T.mn -> Path.t -> E.env -> E.t -> E.t
  let ssize_of_float _ _ _ _ = size 0
  let ssize_of_string _ _ _ _ = size 0
  let ssize_of_bool _ _ _ _ = size 0
  let ssize_of_char _ _ _ _ = size 0
  let ssize_of_i8 _ _ _ _ = size 0
  let ssize_of_i16 _ _ _ _ = size 0
  let ssize_of_i24 _ _ _ _ = size 0
  let ssize_of_i32 _ _ _ _ = size 0
  let ssize_of_i40 _ _ _ _ = size 0
  let ssize_of_i48 _ _ _ _ = size 0
  let ssize_of_i56 _ _ _ _ = size 0
  let ssize_of_i64 _ _ _ _ = size 0
  let ssize_of_i128 _ _ _ _ = size 0
  let ssize_of_u8 _ _ _ _ = size 0
  let ssize_of_u16 _ _ _ _ = size 0
  let ssize_of_u24 _ _ _ _ = size 0
  let ssize_of_u32 _ _ _ _ = size 0
  let ssize_of_u40 _ _ _ _ = size 0
  let ssize_of_u48 _ _ _ _ = size 0
  let ssize_of_u56 _ _ _ _ = size 0
  let ssize_of_u64 _ _ _ _ = size 0
  let ssize_of_u128 _ _ _ _ = size 0
  let ssize_of_tup _ _ _ _ = size 0
  let ssize_of_rec _ _ _ _ = size 0
  let ssize_of_sum _ _ _ _ = size 0
  let ssize_of_vec _ _ _ _ = size 0
  let ssize_of_list _ _ _ _ = size 0
  let ssize_of_null _ _ = size 0
  let ssize_start ?(config=()) _ =
    ignore config ;
    size 0
end
