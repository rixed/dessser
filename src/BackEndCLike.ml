open Batteries
open Stdint
open Dessser
module T = DessserTypes
module E = DessserExpressions

let debug = false

exception Missing_dependencies of string list

type print_state =
  { mutable decl : string IO.output ;
    def : string IO.output ;
    mutable decls : string IO.output list ;
    mutable defs : string IO.output list ;
    mutable indent : string ;
    mutable declared : Set.String.t }

let make_print_state () =
  { decl = IO.output_string () ;
    def = IO.output_string () ;
    decls = [] ;
    defs = [] ;
    indent = "" ;
    declared = Set.String.empty }

let new_top_level p f =
  let p' = make_print_state () in
  let res = f p' in
  (* Merge the new defs and decls into old decls and defs: *)
  p.defs <- p'.def :: p.defs ;
  p.decls <- p'.decl :: p.decls ;
  res

let indent_more p f =
  let indent = p.indent in
  p.indent <- p.indent ^"  " ;
  finally (fun () -> p.indent <- indent)
    f ()

let pp = Printf.fprintf

let declared_type p t f =
  let id = T.uniq_id t in
  if Set.String.mem id p.declared then id
  else (
    p.declared <- Set.String.add id p.declared ;
    (* Write in a temp string to avoid being interrupted by another
     * declaration: *)
    let oc = IO.output_string ()
    and indent = p.indent in
    p.indent <- "" ;
    f oc id ;
    p.indent <- indent ;
    String.print p.decl (IO.close_out oc) ;
    id
  )

type emitter =
  ?name:string ->
  print_state ->
  (E.t * T.t) list ->
  E.t -> (string IO.output -> unit) ->
    string

(* Avoid modifying the name when it's valid: *)
let valid_identifier s =
  if s = "" then "v" else
  if s.[0] >= 'a' && s.[0] <= 'z' || s.[0] = '_' then s else
  "v_"^ s

module type CONFIG =
sig
  val id : T.backend_id
  val valid_identifier : string -> string
  val valid_source_name : string -> string
  val preferred_def_extension : string
  val preferred_decl_extension : string
  val compile_cmd : optim:int -> link:bool -> string -> string -> string

  val type_identifier : print_state -> T.t -> string

  val print_binding :
    string -> string -> ('a IO.output -> unit) -> 'a IO.output -> unit

  val print_inline :
    string -> ('a IO.output -> unit) -> 'a IO.output -> unit

  val print_binding_toplevel :
    emitter -> string -> print_state -> (E.t * T.t) list -> E.t -> unit

  val print_identifier_declaration :
    string -> print_state -> (E.t * T.t) list -> E.t -> unit

  val print_comment : unit IO.output -> ('a, unit IO.output, unit) format -> 'a

  val print : ?name:string -> emitter -> print_state -> (E.t * T.t) list -> E.t -> string

  val source_intro : string

  (* TODO: find a way to factorize the print function itself *)
end

let gen_sym =
  let name_seq = ref (-1) in
  fun pref ->
    incr name_seq ;
    pref ^ string_of_int !name_seq

module Make (C : CONFIG) : BACKEND =
struct
  let id = C.id
  let print_comment = C.print_comment
  let preferred_def_extension = C.preferred_def_extension
  let preferred_decl_extension = C.preferred_decl_extension

  let valid_source_name fname =
    let ext = Filename.extension fname
    and no_ext = Filename.remove_extension fname in
    let basename_no_ext = Filename.basename no_ext
    and dirname = Filename.dirname fname in
    dirname ^"/"^ C.valid_source_name basename_no_ext ^ ext

  let compile_cmd = C.compile_cmd

  let gen_sym () = valid_identifier (gen_sym "id_")

  type identifier =
    { public : bool ; expr : E.t }

  type state =
    { identifiers : (string * identifier * T.t) list ;
      external_identifiers : (string * T.t) list }

  let make_state () =
    { identifiers = [] ; external_identifiers = [] }

  (* Find references to external identifiers: *)
  let get_depends l e =
    E.fold [] l (fun lst l -> function
      | E0 (Identifier s) as e ->
          assert (s <> "") ;
          if List.mem_assoc e l || List.mem s lst then (
            lst
          ) else (
            if debug then
              pp stdout "Expression depends on external identifier %S\n%!" s ;
            s :: lst
          )
      | _ -> lst
    ) e

  let add_external_identifier state name typ =
    { state with external_identifiers =
        (name, typ) :: state.external_identifiers }

  let environment state =
    (* Start with external identifiers: *)
    let l =
      List.map (fun (name, typ) ->
        E.Ops.identifier name, typ
      ) state.external_identifiers in
    (* ...and already defined identifiers in the environment: *)
    let l =
      List.fold_left (fun l (name, _, t) ->
        (E.Ops.identifier name, t) :: l
      ) l state.identifiers in
    l

  let identifier_of_expression state ?name expr =
    let name, public =
      match name with
      | None ->
          gen_sym (), false
      | Some name ->
          name, true in
    let identifier = { public ; expr } in
    let l = environment state in
    E.type_check l expr ;
    let t = E.type_of l expr in
    if t = TVoid then invalid_arg "identifier_of_expression of type void" ;
    { state with
        identifiers = (name, identifier, t) :: state.identifiers },
    E.E0 (Identifier name),
    valid_identifier name

  let find_or_declare_type _p _t =
    assert false

  (* As inlined expressions may be reordered, those must all be stateless.
   * Include in here all operations that are cheap enough that it's OK to
   * compute them several times if required *)
  let rec can_inline = function
    | E.E0 (
        Param _ | Null _ |
        EndOfList _ | Float _ | String _ | Bool _ | Bytes _ | Identifier _ |
        Bit _ | Char _ | Size _ | Byte _ | Word _ | DWord _ | QWord _ | OWord _ |
        U8 _ | U16 _ | U24 _ | U32 _ | U40 _ | U48 _ | U56 _ | U64 _ | U128 _ |
        I8 _ | I16 _ | I24 _ | I32 _ | I40 _ | I48 _ | I56 _ | I64 _ | I128 _) ->
        true
    | E1 ((
        GetItem _ | GetField _ | GetAlt _ | IsNull | ToNullable | ToNotNullable |
        ToU8 | ToU16 | ToU24 | ToU32 | ToU40 | ToU48 | ToU56 | ToU64 | ToU128 |
        ToI8 | ToI16 | ToI24 | ToI32 | ToI40 | ToI48 | ToI56 | ToI64 | ToI128 |
        CharOfPtr | FloatOfPtr | U8OfPtr | I8OfPtr | U16OfPtr | I16OfPtr |
        U24OfPtr | I24OfPtr | U32OfPtr | I32OfPtr | U40OfPtr | I40OfPtr |
        U48OfPtr | I48OfPtr | U56OfPtr | I56OfPtr | U64OfPtr | I64OfPtr |
        U128OfPtr | I128OfPtr | FloatOfQWord | QWordOfFloat | U8OfByte |
        ByteOfU8 | U16OfWord | WordOfU16 | U32OfDWord | DWordOfU32 |
        U64OfQWord | QWordOfU64 | U128OfOWord | OWordOfU128 | U8OfChar |
        CharOfU8 | SizeOfU32 | U32OfSize | BitOfBool | BoolOfBit | U8OfBool |
        BoolOfU8 | LogNot | StringLength | RemSize | Not | Abs | Neg |
        Fst | Snd | Head | Tail | Ignore | Identity), e1) ->
        can_inline e1
    | E2 ((
        Nth | Gt | Ge | Eq | Ne | Add | Sub | Mul | Min | Max |
        LogAnd | LogOr | LogXor | LeftShift | RightShift | GetBit | And |
        Or), e1, e2) ->
        can_inline e1 && can_inline e2
    | _ ->
        false

  let emit ?name p l e f =
    let t = E.type_of l e in
    let tn = C.type_identifier p t in
    if name = None && can_inline e then (
      Printf.sprintf2 "%t" (C.print_inline tn f)
    ) else (
      let n = match name with Some n -> n | None -> gen_sym () in
      pp p.def "%s%t\n" p.indent (C.print_binding n tn f) ;
      n
    )

  let define name p l e =
    let name = valid_identifier name in
    C.print_binding_toplevel emit name p l e

  let declare name p l e =
    let name = valid_identifier name in
    C.print_identifier_declaration name p l e

  let print_source output_identifier state oc =
    (* state is full of identifiers (list of name * exp). Output them in any order
     * as long as dependencies are defined before being used. *)
    let identifiers =
      List.map (fun (name, identifier, _) ->
        let l =
          List.map (fun (name, typ) ->
            E.Ops.identifier name, typ
          ) state.external_identifiers in
        let deps = get_depends l identifier.expr in
        name, deps, identifier.expr
      ) state.identifiers in
    if debug then
      pp stdout "Identifiers:\n%a\n%!"
        (List.print ~first:"" ~last:"" ~sep:"" (fun oc (name, depends, e) ->
          pp oc "  name: %s\n  depends: %a\n  expression: %a\n\n"
            name
            (List.print String.print) depends
            (E.print ?max_depth:None) e)) identifiers ;
    let p = make_print_state () in
    let rec loop progress defined left_overs = function
      | [] ->
          if left_overs <> [] then (
            if not progress then (
              let missings = List.map (fun (n, _, _) -> n) left_overs in
              raise (Missing_dependencies missings)
            ) else loop false defined [] left_overs
          )
      | (name, depends, e) :: rest ->
          let missing_depends =
            List.filter (fun name ->
              not (List.mem_assoc name defined)
            ) depends in
          if missing_depends <> [] then (
            if debug then
              pp stdout "Identifier %s has some undefined dependences, \
                         waiting...\n" name ;
            loop progress defined ((name, missing_depends, e) :: left_overs) rest
          ) else (
            if debug then
              pp stdout "Identifier %s depends on %d defined identifiers, \
                         emitting code...\n" name (List.length depends) ;
            let l =
              List.map (fun (name, t) ->
                E.E0 (Identifier name), t
              ) defined in
            new_top_level p (fun p ->
              output_identifier name p l e) ;
            let t = E.type_of l e in
            let defined = (name, t) :: defined in
            loop true defined left_overs rest
          ) in
    loop false state.external_identifiers [] identifiers ;
    let print_ios oc lst =
      List.rev lst |>
      List.iter (fun io -> String.print oc (IO.close_out io)) in
    Printf.fprintf oc
      "%s\n\n\
       %a\n\
       %a\n\n\
       %a\n\n\
       %a"
      C.source_intro
      C.print_comment "Declarations"
      print_ios p.decls
      C.print_comment "Definitions"
      print_ios p.defs

  let print_definitions state oc =
    print_source define state oc

  let print_declarations state oc =
    print_source declare state oc
end
