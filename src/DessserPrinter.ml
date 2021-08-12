open Batteries

open DessserTools
open DessserMiscTypes
module T = DessserTypes

type context = Declaration | Definition

type t =
  { context : context ;
    def : string IO.output ;
    mutable decls : string list ;
    mutable defs : string list ;
    mutable indent : string ;
    (* The set of all type ids that have already been declared *)
    mutable declared : Set.String.t ;
    mutable external_types : (string * (t -> backend_id -> string)) list ;
    (* Copied from the compilation unit to help the printer to make more
     * educated guesses of type names: *)
    mutable type_names : (T.typ * string) list }

let make ?(declared=Set.String.empty) ?(decls=[]) context type_names external_types =
  { context ;
    def = IO.output_string () ;
    decls ;
    defs = [] ;
    indent = "" ;
    declared ;
    external_types ;
    type_names }

let new_top_level p f =
  let p' = make ~declared:p.declared ~decls:p.decls
                p.context p.type_names p.external_types in
  let res = f p' in
  (* Merge the new defs and decls into old decls and defs: *)
  p.defs <- IO.close_out p'.def :: List.rev_append p'.defs p.defs ;
  p.decls <- p'.decls ;
  p.declared <- Set.String.union p.declared p'.declared ;
  p.external_types <- assoc_merge p.external_types p'.external_types ;
  p.type_names <- assoc_merge p.type_names p'.type_names ;
  res

let indent_more p f =
  let indent = p.indent in
  p.indent <- p.indent ^"  " ;
  finally (fun () -> p.indent <- indent)
    f ()

let type_id p t =
  let t = T.shrink t in
  try list_assoc_eq ~eq:T.eq t p.type_names
  with Not_found -> T.uniq_id t

(* Add the declaration output by [f] before every currently emitted
 * declarations: *)
let prepend_declaration p f =
  (* Write in a temp string to avoid being interrupted by another
   * declaration: *)
  let oc = IO.output_string ()
  and indent = p.indent in
  p.indent <- "" ;
  f oc ;
  p.indent <- indent ;
  p.decls <- IO.close_out oc :: p.decls

(* If [t] has not been declared yet then call [f] that's supposed to emit
 * its declaration (as a type identified by passed identifier). The resulting
 * string is added to declarations.
 * If [t] has already been declared just return its identifier. *)
let declared_type p t f =
  let id = type_id p t in
  if Set.String.mem id p.declared then id
  else (
    p.declared <- Set.String.add id p.declared ;
    prepend_declaration p (fun oc -> f oc id) ;
    id
  )

let declare_if_named p t s f =
  if List.mem_assoc t p.type_names then declared_type p t f
  else s

let get_external_type p name backend_id =
  match List.assoc name p.external_types with
  | exception (Not_found as e) ->
      Printf.eprintf "Unknown external type %S!\n" name ;
      raise e (* reraise *)
  | f ->
      f p backend_id
