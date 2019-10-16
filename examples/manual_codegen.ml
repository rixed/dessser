open Batteries

module BackEnd = Code.BackEnd_C
module E = Code.Expr (BackEnd)

let () =
  let op =
    E.(RemSize
        (AddPointer
           (MakePointer (ConstSize 1000),
                         ConstSize 10))) in
  let output = BackEnd.make_output () in
  let id = E.print_size_expr output op in
  BackEnd.print_output stdout output ;
  Printf.fprintf stdout "return %a;\n" Code.Identifier.Size.print id
