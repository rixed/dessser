open Batteries
open Stdint
open Dessser
module E = DessserExpressions
module P = DessserPrinter
module T = DessserTypes
module U = DessserCompilationUnit

let debug = false

let pp = Printf.fprintf

exception Missing_dependencies of string list

type emitter =
  ?name:string -> P.t -> (E.t * T.t) list -> E.t -> (string IO.output -> unit) ->
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

  val type_identifier : P.t -> T.t -> string

  val print_binding :
    string -> string -> (string IO.output -> unit) -> string IO.output -> unit

  val print_inline :
    P.t -> T.t -> (string IO.output -> unit) -> string IO.output -> unit

  val print_binding_toplevel :
    emitter -> string -> P.t -> (E.t * T.t) list -> E.t -> unit

  val print_identifier_declaration :
    string -> P.t -> (E.t * T.t) list -> E.t -> unit

  val print_comment : 'b IO.output -> ('a, 'b IO.output, unit) format -> 'a

  val print : ?name:string -> emitter -> P.t -> (E.t * T.t) list -> E.t -> string

  val source_intro : string
  val source_outro : string

  (* TODO: find a way to factorize the print function itself *)
end

module Make (C : CONFIG) : BACKEND =
struct
  let id = C.id
  let print_comment = C.print_comment
  let preferred_def_extension = C.preferred_def_extension
  let preferred_decl_extension = C.preferred_decl_extension
  let type_identifier = C.type_identifier

  let valid_source_name fname =
    let ext = Filename.extension fname
    and no_ext = Filename.remove_extension fname in
    let basename_no_ext = Filename.basename no_ext
    and dirname = Filename.dirname fname in
    dirname ^"/"^ C.valid_source_name basename_no_ext ^ ext

  let compile_cmd = C.compile_cmd

  (* Find references to identifiers. Used to order definitions. So does not
   * need to take into account external identifiers, as they are defined
   * outside of dessser's scope. *)
  let get_depends l e =
    E.fold [] l (fun lst l -> function
      | E0 (Identifier s) as e ->
          assert (s <> "") ;
          if List.mem_assoc e l || List.mem s lst then (
            lst
          ) else (
            if debug then
              Printf.fprintf stdout
                "Expression depends on external identifier %S\n%!" s ;
            s :: lst
          )
      | _ -> lst
    ) e

  (* As inlined expressions may be reordered, those must all be stateless.
   * Include in here all operations that are cheap enough that it's OK to
   * compute them several times if required.
   * Arithmetic operators that fail with null are not inlinable (in C++)
   * but none of them count as cheap anyway. *)
  let rec can_inline = function
    | E.E0 (
        Param _ | Null _ |
        EndOfList _ | Float _ | String _ | Bool _ | Bytes _ |
        Identifier _ | ExtIdentifier _ |
        Bit _ | Char _ | Size _ | Byte _ | Word _ | DWord _ | QWord _ | OWord _ |
        U8 _ | U16 _ | U24 _ | U32 _ | U40 _ | U48 _ | U56 _ | U64 _ | U128 _ |
        I8 _ | I16 _ | I24 _ | I32 _ | I40 _ | I48 _ | I56 _ | I64 _ | I128 _) ->
        true
    | E1 ((
        GetItem _ | GetField _ | GetAlt _ | IsNull | NotNull | Force | ToFloat |
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
    if name = None && can_inline e then (
      Printf.sprintf2 "%t" (C.print_inline p t f)
    ) else (
      let n =
        match name with
        | Some n -> n
        | None -> U.gen_sym "id_" |> valid_identifier in
      let tn = C.type_identifier p t in
      Printf.fprintf p.def "%s%t\n" p.indent (C.print_binding n tn f) ;
      n
    )

  let define name p l e =
    let name = valid_identifier name in
    C.print_binding_toplevel emit name p l e

  let declare name p l e =
    let name = valid_identifier name in
    C.print_identifier_declaration name p l e

  let print_intro oc =
    Printf.fprintf oc "%s\n\n" C.source_intro

  let print_source output_identifier compunit p oc =
    (* [compunit] is full of identifiers (list of name * exp).
     * Output them in any order as long as dependencies are defined before
     * being used. *)
    let identifiers =
      List.map (fun (_, identifier, _ as id) ->
        let l =
          List.map (fun (name, typ) ->
            E.Ops.identifier name, typ
          ) compunit.U.external_identifiers in
        let deps = get_depends l identifier.U.expr in
        id, deps
      ) compunit.U.identifiers in
    if debug then
      Printf.fprintf stdout "Identifiers:\n%a\n%!"
        (List.print ~first:"" ~last:"" ~sep:"" (fun oc ((name, id, _), depends) ->
          pp oc "  name: %s\n  depends: %a\n  expression: %a\n\n"
            name
            (List.print String.print) depends
            (E.print ?max_depth:None) id.U.expr)) identifiers ;
    let rec loop progress defined left_overs = function
      | [] ->
          if left_overs <> [] then (
            if not progress then (
              let missings =
                List.fold_left (fun l (_, deps) ->
                  List.rev_append deps l
                ) [] left_overs in
              raise (Missing_dependencies missings)
            ) else loop false defined [] left_overs
          )
      | ((name, U.{ expr ; _ }, _ as id), depends) :: rest ->
          let missing_depends =
            List.filter (fun name ->
              not (List.mem_assoc name compunit.U.external_identifiers) &&
              not (List.exists (fun (n, _, _) -> n = name) defined)
            ) depends in
          if missing_depends <> [] then (
            if debug then
              Printf.fprintf stdout "Identifier %s has some undefined \
                                     dependences, waiting...\n" name ;
            let left_overs' = (id, missing_depends) :: left_overs in
            loop progress defined left_overs' rest
          ) else (
            if debug then
              Printf.fprintf stdout "Identifier %s depends on %d defined \
                                     identifiers, emitting code...\n"
                name (List.length depends) ;
            let l = U.environment U.{ compunit with identifiers = defined } in
            P.new_top_level p (fun p ->
              output_identifier name p l expr) ;
            let defined = id :: defined in
            loop true defined left_overs rest
          ) in
    loop false [] [] identifiers ;
    let print_ios oc lst =
      List.rev lst |>
      List.iter (fun io ->
        Printf.fprintf oc "%s\n" (IO.close_out io)) in
    Printf.fprintf oc
      "%a\n\
       %a\n\n\
       %a\n\n\
       %a\n\n\
       %s\n"
      C.print_comment "Declarations"
      print_ios p.decls
      C.print_comment "Definitions"
      print_ios p.defs
      C.source_outro

  let print_verbatim location defs p oc =
    List.rev defs |>
    List.iter (fun (backend, loc, f) ->
      if loc = location && backend = id then
        Printf.fprintf oc "%t" (f p))

  let print_definitions compunit oc =
    print_intro oc ;
    let p = P.make () in
    print_verbatim U.Top compunit.U.verbatim_definitions p oc ;
    print_source define compunit p oc ;
    print_verbatim U.Bottom compunit.U.verbatim_definitions p oc

  let print_declarations compunit oc =
    print_intro oc ;
    let p = P.make () in
    print_source declare compunit p oc
end
