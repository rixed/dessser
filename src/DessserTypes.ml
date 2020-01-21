open Batteries
open Stdint

let pp = Printf.fprintf

(* Basic scalar types that can be used to define more specialized user types *)

type mac_type =
  | TFloat | TString | TBool | TChar
  | TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128
  | TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128

(* Machine-types are the basic types from which more specialized types can be
 * build. Those constructed types are called user-types.
 * User types specialize machine types in several ways: they have their own
 * name, pretty-printer, parser, and of course an implementation as the
 * corresponding machine type.
 * Notice that user types cannot be parameterized (yet). So only scalar user
 * types are possible for now.
 * User expressions can restrict types to user types. *)

and user_type =
  { name : string ;
    print : 'a. 'a IO.output -> unit ;
    (* parse *)
    def : value_type }

(* Those types describing values that can be (de)serialized.
 * All of them can possibly be nullable.
 * User types are build from those value types. *)

and value_type =
  | Mac of mac_type
  | Usr of user_type
  (* Compound types: *)
  | TVec of int * maybe_nullable
  | TList of maybe_nullable
  | TTup of maybe_nullable array
  (* Exact same as a tuple, but with field names that can be used as
   * accessors (also used to name actual fields in generated code): *)
  | TRec of (string * maybe_nullable) array
  (* The type for maps exist because there will be some operations using
   * that type indirectly (such as fetching from a DB by key, or describing
   * a key->value mapping in a type expression). But there is no value of
   * that type, ever. From a (de)serialized point of view, maps are
   * equivalent to association lists. *)
  | TMap of maybe_nullable * maybe_nullable
  (* TODO: sum types *)

and maybe_nullable =
  | Nullable of value_type
  | NotNullable of value_type

let is_nullable = function
  | Nullable _ -> true
  | NotNullable _ -> false

let to_value_type = function
  | Nullable t -> t
  | NotNullable t -> t

let to_nullable = function
  | NotNullable t -> Nullable t
  | Nullable _ as x -> x

let print_mac_type oc =
  let sp = String.print oc in
  function
  | TFloat -> sp "Float"
  | TString -> sp "String"
  | TBool -> sp "Bool"
  | TChar -> sp "Char"
  | TU8 -> sp "U8"
  | TU16 -> sp "U16"
  | TU24 -> sp "U24"
  | TU32 -> sp "U32"
  | TU40 -> sp "U40"
  | TU48 -> sp "U48"
  | TU56 -> sp "U56"
  | TU64 -> sp "U64"
  | TU128 -> sp "U128"
  | TI8 -> sp "I8"
  | TI16 -> sp "I16"
  | TI24 -> sp "I24"
  | TI32 -> sp "I32"
  | TI40 -> sp "I40"
  | TI48 -> sp "I48"
  | TI56 -> sp "I56"
  | TI64 -> sp "I64"
  | TI128 -> sp "I128"

let rec print_value_type oc = function
  | Mac t ->
      print_mac_type oc t
  | Usr t ->
      t.print oc
  | TVec (dim, mt) ->
      pp oc "%a[%d]" print_maybe_nullable mt dim
  | TList mt ->
      pp oc "%a[]" print_maybe_nullable mt
  | TTup mts ->
      pp oc "%a"
        (Array.print ~first:"(" ~last:")" ~sep:";" print_maybe_nullable) mts
  | TRec mts ->
      pp oc "%a"
        (Array.print ~first:"{" ~last:"}" ~sep:";"
          (fun oc (n, t) ->
            pp oc "%s: %a" n print_maybe_nullable t)
        ) mts
  | TMap (k, v) ->
      pp oc "%a{%a}"
        print_maybe_nullable v
        print_maybe_nullable k

and print_maybe_nullable oc = function
  | Nullable t ->
      pp oc "%a?" print_value_type t
  | NotNullable t ->
      print_value_type oc t

let user_types = Hashtbl.create 50

type gen_printer = { f : 'a. 'a IO.output -> unit }

let register_user_type name ?(print : gen_printer option) def =
  Hashtbl.modify_opt name (function
    | None ->
        let print = match print with
          | Some printer -> printer.f
          | None -> fun oc -> print_value_type oc def in
        Some { name ; print ; def }
    | Some _ ->
        invalid_arg "register_user_type"
  ) user_types

let get_user_type = Hashtbl.find user_types

(* Examples: *)
let () =
  register_user_type "Date" (Mac TFloat) ;
  register_user_type "Eth" (Mac TU48) ;
  register_user_type "Ipv4" (Mac TU32) ;
  register_user_type "Ipv6" (Mac TU128) ;
  register_user_type "Cidrv4" (TTup [| NotNullable (Usr (get_user_type "Ipv4")) ;
                                       NotNullable (Mac TU8) |]) ;
  register_user_type "Cidrv6" (TTup [| NotNullable (Usr (get_user_type "Ipv6")) ;
                                       NotNullable (Mac TU8) |])

(* Paths are used to locate subfields within compound types. *)
type path = int list

let print_path oc p =
  List.print ~first:"" ~last:"" ~sep:"/" Int.print oc p

let rec type_of_path t path =
  match path with
  | [] -> t
  | i :: path ->
      let rec type_of_not_nullable = function
        | NotNullable (Mac _ | TMap _) ->
            assert false
        | NotNullable (Usr t) ->
            type_of_not_nullable (NotNullable t.def)
        | NotNullable (TVec (dim, mt)) ->
            assert (i < dim) ;
            type_of_path mt path
        | NotNullable (TList mt) ->
            type_of_path mt path
        | NotNullable (TTup mts) ->
            assert (i < Array.length mts) ;
            type_of_path mts.(i) path
        | NotNullable (TRec mts) ->
            assert (i < Array.length mts) ;
            type_of_path (snd mts.(i)) path
        | Nullable x ->
            type_of_not_nullable (NotNullable x) |>
            to_nullable in
      type_of_not_nullable t

(*$inject
   let test_t = NotNullable (TTup [| NotNullable (Mac TU8) ; 
                                     Nullable (Mac TString) |])
*)

(*$= type_of_path & ~printer:(BatIO.to_string print_maybe_nullable)
  test_t (type_of_path test_t [])
  (NotNullable (Mac TU8)) (type_of_path test_t [0])
  (Nullable (Mac TString)) (type_of_path test_t [1])
*)

(* To all the above types we add a few low-level types that can not be used
 * in values but are useful to manipulate them. *)
type typ =
  | TValue of maybe_nullable
  | TVoid
  (* DataPtr are used to point into the stream of bytes that's being
   * serialized into / deserialized from. The type of the value that's
   * being (de)serialized is kept nonetheless. *)
  | TDataPtr
  (* ValuePtr are used to point at heap allocated values of a given type.
   * The "offset" is then the location in that data structure, and it is
   * "advanced" by hopping from subfield to subfields, traversing the
   * structure depth first. The path is thus merely an integer, but the
   * backend has to know how to locate each addressable leaves. *)
  | TValuePtr of maybe_nullable
  (* A size in byte. *)
  | TSize
  (* Data access, may be just pointer to the actual serialized object: *)
  | TBit
  | TByte
  | TWord
  | TDWord
  | TQWord
  | TOWord
  | TBytes
  | TPair of typ * typ
  | TFunction of typ array * (* result: *) typ

let rec print_typ oc =
  let sp = String.print oc in
  function
  | TValue mt ->
      print_maybe_nullable oc mt
  | TVoid -> sp "Void"
  | TDataPtr -> sp "DataPtr"
  | TValuePtr t ->
      pp oc "ValuePtr(%a)" print_maybe_nullable t
  | TSize -> sp "Size"
  | TBit -> sp "Bit"
  | TByte -> sp "Byte"
  | TWord -> sp "Word"
  | TDWord -> sp "DWord"
  | TQWord -> sp "QWord"
  | TOWord -> sp "OWord"
  | TBytes -> sp "Bytes"
  | TPair (t1, t2) ->
      pp oc "Pair(%a, %a)"
        print_typ t1
        print_typ t2
  | TFunction ([||], t1) ->
      pp oc "(unit->%a)" print_typ t1
  | TFunction (ts, t2) ->
      pp oc "(%a->%a)"
        (Array.print ~first:"" ~last:"" ~sep:"->" print_typ) ts
        print_typ t2

let typ_to_nullable = function
  | TValue (NotNullable t) -> TValue (Nullable t)
  | t ->
      Printf.eprintf "Cannot turn type %a into nullable\n%!"
        print_typ t ;
      assert false

let typ_to_not_nullable = function
  | TValue (Nullable t) -> TValue (NotNullable t)
  | t ->
      Printf.eprintf "Cannot turn type %a into not-nullable\n%!"
        print_typ t ;
      assert false

(* Some short cuts for often used types: *)

let bool = TValue (NotNullable (Mac TBool))
let char = TValue (NotNullable (Mac TChar))
let nstring = TValue (Nullable (Mac TString))
let string = TValue (NotNullable (Mac TString))
let float = TValue (NotNullable (Mac TFloat))
let u8 = TValue (NotNullable (Mac TU8))
let u16 = TValue (NotNullable (Mac TU16))
let u24 = TValue (NotNullable (Mac TU24))
let u32 = TValue (NotNullable (Mac TU32))
let u40 = TValue (NotNullable (Mac TU40))
let u48 = TValue (NotNullable (Mac TU48))
let u56 = TValue (NotNullable (Mac TU56))
let u64 = TValue (NotNullable (Mac TU64))
let u128 = TValue (NotNullable (Mac TU128))
let i8 = TValue (NotNullable (Mac TI8))
let i16 = TValue (NotNullable (Mac TI16))
let i24 = TValue (NotNullable (Mac TI24))
let i32 = TValue (NotNullable (Mac TI32))
let i40 = TValue (NotNullable (Mac TI40))
let i48 = TValue (NotNullable (Mac TI48))
let i56 = TValue (NotNullable (Mac TI56))
let i64 = TValue (NotNullable (Mac TI64))
let i128 = TValue (NotNullable (Mac TI128))
let void = TVoid
let bit = TBit
let byte = TByte
let size = TSize
let word = TWord
let dword = TDWord
let qword = TQWord
let oword = TOWord
let bytes = TBytes
let dataptr = TDataPtr
let valueptr t = TValuePtr t
let pair t1 t2 = TPair (t1, t2)
