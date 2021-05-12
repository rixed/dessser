open Batteries

open DessserTools
module T = DessserTypes

type context = Declaration | Definition

type t =
  { context : context ;
    decl : string IO.output ;
    def : string IO.output ;
    mutable decls : string IO.output list ;
    mutable defs : string  IO.output list ;
    mutable indent : string ;
    mutable declared : Set.String.t ;
    mutable external_types : (string * (t -> T.backend_id -> string)) list ;
    (* Copied from the compilation unit to help the printer to make more
     * educated guesses of type names: *)
    mutable type_names : (T.value * string) list }

let make ?(declared=Set.String.empty) context type_names external_types =
  { context ;
    decl = IO.output_string () ;
    def = IO.output_string () ;
    decls = [] ;
    defs = [] ;
    indent = "" ;
    declared ;
    external_types ;
    type_names }

let new_top_level p f =
  let p' = make ~declared:p.declared p.context p.type_names p.external_types in
  let res = f p' in
  (* Merge the new defs and decls into old decls and defs: *)
  p.defs <- p'.def :: List.rev_append p'.defs p.defs ;
  p.decls <- p'.decl :: List.rev_append p'.decls p.decls ;
  p.declared <- Set.String.union p.declared p'.declared ;
  p.external_types <- assoc_merge p.external_types p'.external_types ;
  p.type_names <- assoc_merge p.type_names p'.type_names ;
  res

let indent_more p f =
  let indent = p.indent in
  p.indent <- p.indent ^"  " ;
  finally (fun () -> p.indent <- indent)
    f ()

let declared_type p t f =
  let id =
    match t with
    | T.Data { vtyp ; _ } ->
        (try List.assoc vtyp p.type_names
        with Not_found -> T.uniq_id t)
    | _ ->
        T.uniq_id t in
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

let get_external_type p name backend_id =
  match List.assoc name p.external_types with
  | exception (Not_found as e) ->
      Printf.eprintf "Unknown external type %S!\n" name ;
      raise e (* reraise *)
  | f ->
      f p backend_id
