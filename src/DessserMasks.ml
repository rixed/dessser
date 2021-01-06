(* A Mask is a way to select into a type a subset of its values.
 * Of course values have depth, so do masks.
 *
 * Masks can be represented as a text string such as "__XX(X_X)_", meaning to
 * skip the first two subcomponents, select the next two, open the fifth and
 * take the first and third subitems, then finally skip the sixth and last
 * item.
 *
 * Values can be replaced by immediate values using the notation "[val]" where
 * "val" must be an s-expression of the same type as the value being replaced.
 *
 * Values can also be inserted by "{val}" where "val" is an s-expression.
 *
 * In case a type has more items than specified in the mask then the following
 * items will be skipped or copied depending on the last mask action. If the
 * last mask action is neither copy nor skip then the remaining fields will
 * be skipped.
 * It is an error if a type has fewer fields than the type it is applied to.
 *
 * From a mask and a type, Dessser can generate the resulting type of applying
 * the mask to that type.
 * There also exist a runtime representation of fieldmasks that can be used
 * when serializing a HeapValue.
 *
 * For these runtime masks, field insertion is not yet supported, and
 * replacement of value can only replace a value to NULL.
 *
 * Basically this is currently only the minimum required by Ramen.
 * TODO:
 * - Support Insert/Replace in runtime fieldmasks;
 * - Generating a function to project a HeapValue into another one;
 * - Apply a runtime fieldmask when deserializing a heap value;
 * - Apply a runtime fieldmask in dessser for any format;
 * - Generate more efficient proj/ser/des for when the fieldmask is given at
 *   compile time.
 *)
open Batteries
open DessserTools
module T = DessserTypes
module E = DessserExpressions
open E.Ops

type t = action array

and action =
  | Copy (* Copy this item (and any subitems) *)
  | Skip (* Skip this item (and any subitems) *)
  | SetNull (* Same as Replace with a null value, with no need to specify the type *)
  | Recurse of t (* Apply the given mask to the subitems *)
  | Replace of E.t (* Replace this item by a constant value of the same type *)
  | Insert of E.t (* Insert this constant value at this location *)

let rec action_eq a1 a2 =
  match a1, a2 with
  | Copy, Copy
  | Skip, Skip
  | SetNull, SetNull ->
      true
  | Recurse t1, Recurse t2 ->
      eq t1 t2
  | Replace e1, Replace e2
  | Insert e1, Insert e2 ->
      E.eq e1 e2
  | _ ->
      false

and eq t1 t2 =
  try
    Array.for_all2 (fun a1 a2 -> action_eq a1 a2) t1 t2
  with Invalid_argument _ ->
    false

let all_skips m =
  Array.for_all (fun ma -> ma = Skip) m

let rec string_of_action = function
  | Copy -> "X"
  | Skip -> "_"
  | SetNull -> "N"
  | Recurse m -> "("^ string_of_mask m ^")"
  | Replace e -> "["^ E.to_string e ^"]"
  | Insert e -> "{"^ E.to_string e ^"}"

and string_of_mask m =
  (Array.enum m /@ string_of_action) |> List.of_enum |>
  String.join ""

let print_action oc ma =
  String.print oc (string_of_action ma)

let print_mask oc m =
  String.print oc (string_of_mask m)

module Parser =
struct
  (*$< Parser *)

  exception Missing_end_of_recurse of int
  exception Missing_end_of_replace of int
  exception Missing_end_of_insert of int
  exception Invalid_expression of int
  exception Invalid_character of int
  exception Garbage_after of int

  (* Return the next action and the next offset in [str] *)
  let rec action str i =
    assert (i < String.length str) ;
    let e_of_toks toks =
      match E.Parser.expr_of_toks toks str with
      | [ e ] -> e
      | _ -> raise (Invalid_expression i)
    in
    let c = str.[i] in
    if c = 'X' || c = 'x' then Copy, i + 1
    else if c = '_' || c = '-' then Skip, i + 1
    else if c = 'N' || c = 'n' then SetNull, i + 1
    else if c = '(' then
      let m, i = mask str (i + 1) in
      if i >= String.length str || str.[i] <> ')' then
        raise (Missing_end_of_recurse i)
      else if all_skips m then Skip, i + 1
      else
        Recurse m, i + 1
    else if c = '[' then
      let toks, i = E.Parser.tok str [] (i + 1) in
      if i >= String.length str || str.[i] <> ']' then
        raise (Missing_end_of_replace i)
      else
        Replace (e_of_toks toks), i + 1
    else if c = '{' then
      let toks, i = E.Parser.tok str [] (i + 1) in
      if i >= String.length str || str.[i] <> '}' then
        raise (Missing_end_of_insert i)
      else
        Insert (e_of_toks toks), i + 1
    else
      raise (Invalid_character i)

  and mask str i =
    let to_array = Array.of_list % List.rev in
    let rec loop prev i =
      if i >= String.length str then to_array prev, i else
      match action str i with
      | exception Invalid_character i ->
          to_array prev, i
      | a, i ->
          loop (a :: prev) i
    in
    loop [] i

  let action_of_string str =
    let ma, i = action str 0 in
    if i < String.length str then
      raise (Garbage_after i)
    else
      ma

  let mask_of_string str =
    let m, i = mask str 0 in
    if i < String.length str then
      raise (Garbage_after i)
    else
      m

  (*$= mask_of_string & ~printer:string_of_mask
    [| Copy ; Copy ; Copy |] (mask_of_string "xxx")
    [| Copy ; Recurse [| Skip ; Insert (E.(E0 (Null T.(Mac U8)))) ; Copy |] |] \
        (mask_of_string "X(_{(null \"u8\")}X)")
    [| Copy ; Recurse [| Skip ; SetNull ; Copy |] |] (mask_of_string "X(_NX)")
  *)

  (*$>*)
end

exception Types_do_not_match of { mask : T.t ; expr : T.t }
exception Invalid_type_for_mask of T.t
exception Mask_too_long_for_type of T.maybe_nullable * t
exception Not_a_recursive_type of T.maybe_nullable
exception Cannot_skip_that
exception Cannot_insert_into_that
exception Cannot_set_null of T.maybe_nullable

(* Project type [mn] according to mask action [ma]: *)
let rec project mn ma =
  let recurse_record mn mns m =
    let mns, _, _ =
      (* [n] count the inserted fields while [i] is the index in [mns] *)
      Array.fold_left (fun (mns', n, i) ma ->
        if i >= Array.length mns then
          raise (Mask_too_long_for_type (mn, m)) ;
        match ma with
        | Skip -> mns', n, i + 1
        | Insert e ->
            (match E.type_of [] e with
            | T.Value mn ->
                ("inserted_"^ Stdlib.string_of_int n, mn) :: mns', n + 1, i
            | t -> raise (Invalid_type_for_mask t))
        | ma ->
            let name, mn = mns.(i) in
            (name, project mn ma) :: mns', n, i + 1
      ) ([], 0, 0) m in
    (* In theory recursing into all skips should have been replaced by skip
     * already when parsing: *)
    if mns = [] then raise Cannot_skip_that ;
    Array.of_list (List.rev mns) in
  let recurse_tuple mn mns m =
    let mns = Array.map (fun mn -> "", mn) mns in
    let mns = recurse_record mn mns m in
    Array.map snd mns in
  match ma with
  | Copy ->
      mn
  | Skip ->
      raise Cannot_skip_that
  | SetNull ->
      if mn.T.nullable then
        mn (* Does not change the type *)
      else
        raise (Cannot_set_null mn)
  | Recurse m ->
      (match mn.T.vtyp with
      | T.Tup mns ->
          let mns = recurse_tuple mn mns m in
          (* No tuples of 1 items: *)
          if Array.length mns = 1 then mns.(0) else
            { mn with vtyp = T.Tup mns }
      | T.Rec mns ->
          let mns = recurse_record mn mns m in
          (* A record of onw field is OK: *)
          { mn with vtyp = T.Rec mns }
      | _ ->
          raise (Not_a_recursive_type mn))
  | Replace e ->
      let te = E.type_of [] e in
      if te = Value mn then mn (* Does not change the type *)
      else raise (Types_do_not_match { expr = te ; mask = T.Value mn })
  | Insert _ ->
      raise Cannot_insert_into_that


(*$inject
  let s2t = T.maybe_nullable_of_string
  let s2a = Parser.action_of_string *)

(*$= project & ~printer:(BatIO.to_string T.print_maybe_nullable)
  (T.optional (Mac U8)) (* Do nothing case *) \
    (project (T.optional (Mac U8)) Copy)
  (s2t "u8?") (* Same as above but using the textual representation *) \
    (project (s2t "u8?") (s2a "X"))
  (s2t "u8?") (* Not "(u8?)"! *) \
    (project (s2t "(string; u8?; char)") (s2a "(_X_)"))
  (s2t "(string; char)") (* Projecting away the whole mid structure: *) \
    (project (s2t "(string; { foo: u8?; bar: u8 }; char)") (s2a "(X(__)X)"))
  (s2t "(string; char)") (* Inserting an item: *) \
    (project (s2t "(string; string)") (s2a "(X{(char \"c\")}_)"))
  (s2t "{ foo: u8; bar: char? }") (* Changing a value does not change the type: *) \
    (project (s2t "{ foo: u8; bar: char? }") (s2a "(X[(not-null (char \"c\"))])"))
*)


(* TODO: given a type and a mask, build the function that takes a heap
 * value of that type and return a heap value of the projection:
let projector mn m =
  fun (e : E.t) : E.t ->
    fold ~copy ~skip ~replace ~insert ~enter ~leave mn m e
*)

let () =
  Printexc.register_printer (function
    | Parser.Missing_end_of_recurse i ->
        Some ("Missing ')' mark at position "^ Stdlib.string_of_int i)
    | Parser.Missing_end_of_replace i ->
        Some ("Missing ']' mark at position "^ Stdlib.string_of_int i)
    | Parser.Missing_end_of_insert i ->
        Some ("Missing '}' mark at position "^ Stdlib.string_of_int i)
    | Parser.Invalid_expression i ->
        Some ("Invalid expression for mask before "^ Stdlib.string_of_int i)
    | Parser.Invalid_character i ->
        Some ("Invalid character at position "^ Stdlib.string_of_int i)
    | Parser.Garbage_after i ->
        Some ("Cannot parse mask after position "^ Stdlib.string_of_int i)
    | Types_do_not_match { mask ; expr } ->
        Some (
          Printf.sprintf2 "Type in mask (%a) does not match masked type (%a)"
            T.print mask T.print expr)
    | Invalid_type_for_mask t ->
        Some (
          Printf.sprintf2 "Masks can not use type %a, only maybe_nullable types"
            T.print t)
    | Mask_too_long_for_type (mn, t) ->
        Some (
          Printf.sprintf2 "Mask %a is too long for type %a"
            print_mask t T.print_maybe_nullable mn)
    | Not_a_recursive_type mn ->
        Some (
          Printf.sprintf2 "Cannot recurse into type %a"
            T.print_maybe_nullable mn)
    | Cannot_skip_that ->
        Some "Can only skip tuple or record fields"
    | Cannot_insert_into_that ->
        Some "Can insert fields only in tuples or records"
    | Cannot_set_null mn ->
        Some (
          Printf.sprintf2 "Cannot force not nullable type %a to null"
            T.print_maybe_nullable mn)
    | _ ->
        None)
