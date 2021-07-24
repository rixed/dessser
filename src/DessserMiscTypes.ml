module String = BatString

(* Identifies the backend implementation: *)

type backend_id = DIL | OCaml | Cpp

(* Identifies the supported encodings: *)

type encoding_id = User of string | RingBuff | RowBinary | SExpr | CSV | JSON | Null

let string_of_encoding = function
  | User s -> s
  | Null -> "null"
  | RingBuff -> "ringbuf"
  | RowBinary -> "row-binary"
  | SExpr -> "s-expression"
  | CSV -> "csv"
  | JSON -> "json"

let encoding_of_string s =
  match String.lowercase_ascii s with
  | "null" -> Null
  | "ringbuf" -> RingBuff
  | "row-binary" -> RowBinary
  | "s-expression" -> SExpr
  | "csv" -> CSV
  | "json" -> JSON
  | s -> User s

(* Identifies the various kind of set implementations: *)

type set_type =
  Simple | Sliding | Tumbling | Sampling | HashTable | Heap | Top

let string_of_set_type = function
  | Simple -> ""
  | Sliding -> "sliding"
  | Tumbling -> "tumbling"
  | Sampling -> "sampling"
  | HashTable -> "hashtable"
  | Heap -> "heap"
  | Top -> "top"

(* Endianness of some operations: *)

type endianness = LittleEndian | BigEndian

let string_of_endianness = function
  | LittleEndian -> "little-endian"
  | BigEndian -> "big-endian"

let print_endianness oc en =
  string_of_endianness en |> String.print oc

let endianness_of_string = function
  | "little-endian" -> LittleEndian
  | "big-endian" -> BigEndian
  | en -> invalid_arg ("endianness_of_string "^ en)

(* Identifies the known types of external functions: *)

type type_method =
  | SerWithMask of encoding_id  (* serialize into this encoding *)
  | SerNoMask of encoding_id
  (* TODO: DesWithMask *)
  | DesNoMask of encoding_id  (* deserialize from this encoding *)
  | SSizeWithMask of encoding_id  (* serialized size in this encoding *)
  | SSizeNoMask of encoding_id
  | Convert of encoding_id * encoding_id  (* convert from a to b encodings *)

let string_of_type_method = function
  | SerWithMask enc ->
      "to-"^ string_of_encoding enc ^"-with-mask"
  | SerNoMask enc ->
      "to-"^ string_of_encoding enc
  | DesNoMask enc ->
      "of-"^ string_of_encoding enc
  | SSizeWithMask enc ->
      "sersize-of-"^ string_of_encoding enc ^"-with-mask"
  | SSizeNoMask enc ->
      "sersize-of-"^ string_of_encoding enc
  | Convert (from_, to_) ->
      string_of_encoding to_ ^"-of-"^ string_of_encoding from_

let type_method_of_string s =
  let to_enc n s = encoding_of_string (String.lchop ~n s) in
  let s = String.lowercase_ascii s in
  if String.starts_with s "to-" && String.ends_with s "-with-mask" then
    SerWithMask (to_enc 3 s) else
  if String.starts_with s "to-" then
    SerNoMask (to_enc 3 s) else
  if String.starts_with s "of-" then
    DesNoMask (to_enc 3 s) else
  if String.starts_with s "sersize-of-" && String.ends_with s "-with-mask" then
    SSizeWithMask (to_enc 11 s) else
  if String.starts_with s "sersize-of-" then
    SSizeNoMask (to_enc 11 s) else
  match String.split ~by:"-of-" s with
  | exception Not_found ->
      invalid_arg ("type_method_of_string: "^ s)
  | from_, to_ -> Convert (to_enc 0 from_, to_enc 0 to_)

type ext_identifier =
  | Verbatim of string (* Used as is by any back-end, no question asked *)
  | Method of { typ : string ; meth : type_method }
