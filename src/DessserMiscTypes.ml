(* Identifies the backend implementation: *)

type backend_id = DIL | OCaml | Cpp

(* Identifies the supported encodings: *)

type encoding_id = User of string | RingBuff | RowBinary | SExpr | CSV | Null

let string_of_encoding = function
  | User s -> s
  | Null -> "null"
  | RingBuff -> "ringbuf"
  | RowBinary -> "row-binary"
  | SExpr -> "s-expression"
  | CSV -> "csv"

let encoding_of_string s =
  match BatString.lowercase_ascii s with
  | "null" -> Null
  | "ringbuf" -> RingBuff
  | "row-binary" -> RowBinary
  | "s-expression" -> SExpr
  | "csv" -> CSV
  | s -> User s

(* Identifies the various kind of set implementations: *)

type set_type =
  Simple | Sliding | Tumbling | Sampling | HashTable | Heap | Top
