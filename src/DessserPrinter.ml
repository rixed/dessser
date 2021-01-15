open Batteries
module T = DessserTypes

type t =
  { mutable decl : string IO.output ;
    def : string IO.output ;
    mutable decls : string IO.output list ;
    mutable defs : string  IO.output list ;
    mutable indent : string ;
    mutable declared : Set.String.t }

let make ?(declared=Set.String.empty) () =
  { decl = IO.output_string () ;
    def = IO.output_string () ;
    decls = [] ;
    defs = [] ;
    indent = "" ;
    declared }

let new_top_level p f =
  let p' = make ~declared:p.declared () in
  let res = f p' in
  (* Merge the new defs and decls into old decls and defs: *)
  p.defs <- p'.def :: p.defs ;
  p.decls <- p'.decl :: p.decls ;
  p.declared <- Set.String.union p.declared p'.declared ;
  res

let indent_more p f =
  let indent = p.indent in
  p.indent <- p.indent ^"  " ;
  finally (fun () -> p.indent <- indent)
    f ()

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
