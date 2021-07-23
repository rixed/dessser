open Batteries
open Stdint

open DessserMiscTypes
open DessserTools
module T = DessserTypes
module E = DessserExpressions
module Printer = DessserPrinter
module Path = DessserPath
module StdLib = DessserStdLib
module U = DessserCompilationUnit

(* Used by deserializers to "open" arrs: *)
type arr_opener =
  (* When a arr size is known from the beginning, implement this that
   * returns both the arr size and the new src pointer: *)
  | KnownSize of (T.mn -> T.mn -> Path.t -> (*ptr*) E.t -> (* (u32 * ptr) *) E.t)
  (* Whereas when the arr size is not known beforehand, rather implement
   * this pair of functions, one to parse the arr header and return the new
   * src pointer and one that will be called before any new token and must
   * return true if the arr is finished: *)
  | UnknownSize of
      (T.mn -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t) *
      (T.mn -> Path.t -> (*ptr*) E.t -> (*bool*) E.t)

(* Given the above we can built interesting expressions.
 * A DES(serializer) is a module that implements a particular serialization
 * format in such a way that it provides simple expressions for the basic
 * types that can then be assembled to build either a deserializer from
 * DataPtr to a heap value, or a converter between two formats. *)
module type DES =
sig
  val id : encoding_id

  (* DES and SER can have some configurable parameters: *)
  type config

  (* No need for a backend (BE) since we merely compute expressions *)
  (* RW state passed to every deserialization operations *)
  type state
  (* Wraps a DataPtr into whatever the DES needs *)
  val ptr : T.mn -> T.mn

  val start : ?config:config -> T.mn -> (*dataptr*) E.t ->
              state * (*ptr*) E.t
  val stop : state -> (*ptr*) E.t -> (*ptr*) E.t

  (* A basic value deserializer takes a state, an expression
   * yielding a pointer (either a CodePtr pointing at a byte stream or a
   * ValuePtr pointing at a heap value of type ['b]), and returns two
   * expressions: one yielding the advanced pointer (of the exact same type) and
   * one yielding the value that's been deserialized from the given location.
   * The [maybe_nullable] and [path] values are in the fully fledged global
   * type and therefore must be used cautiously! *)
  (* FIXME: make this type "private": *)
  type des = state -> T.mn -> Path.t -> (*ptr*) E.t -> (*v*ptr*) E.t

  val dfloat : des
  val dstring : des
  val dbool : des
  val dchar : des
  val di8 : des
  val di16 : des
  val di24 : des
  val di32 : des
  val di40 : des
  val di48 : des
  val di56 : des
  val di64 : des
  val di128 : des
  val du8 : des
  val du16 : des
  val du24 : des
  val du32 : des
  val du40 : des
  val du48 : des
  val du56 : des
  val du64 : des
  val du128 : des
  (* The callback receives and returns mere T.ptr instead of the ptr used for
   * desser: *)
  val dext : ((*ptr*) E.t -> (*V*ptr*) E.t) -> des

  (* Paths passed to opn/cls/sep functions are the path of the compound structure
   * itself *)

  (* Get rid of the _seps (in SExpr, use a state to add a separator before any
   * value instead). That would make the code generated for dessser significantly
   * simpler, and even more so when runtime fieldmasks enter the stage! *)
  val tup_opn : T.mn array ->
                state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  val tup_cls : state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  val tup_sep : state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  val rec_opn : (string * T.mn) array ->
                state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  val rec_cls : state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  val rec_sep : state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  (* Returns the label as an u16 and the new pointer: *)
  val sum_opn : (string * T.mn) array ->
                state -> T.mn -> Path.t -> (*ptr*) E.t -> (* u16*ptr *) E.t
  val sum_cls : (*u16*) E.t ->
                state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  val vec_opn : (*dim*) int -> T.mn ->
                state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  val vec_cls : state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  val vec_sep : state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  val arr_opn : state -> arr_opener
  val arr_cls : state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  val arr_sep : state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t

  val is_null : state -> T.mn -> Path.t -> (*ptr*) E.t -> (*bool*) E.t
  val dnull : T.t ->
                state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  val dnotnull : T.t ->
                state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
end

module type SER =
sig
  val id : encoding_id

  (* DES and SER can have some configurable parameters: *)
  type config

  (* RW state passed to every serialization operations *)
  type state
  (* Wraps a DataPtr into whatever the SER needs *)
  val ptr : T.mn -> T.mn

  val start : ?config:config -> T.mn -> (*dataptr*) E.t ->
              state * (*ptr*) E.t
  val stop : state -> (*ptr*) E.t -> (*ptr*) E.t

  (* FIXME: make this type "private": *)
  type ser = state -> T.mn -> Path.t -> (*v*) E.t -> (*ptr*) E.t -> (*ptr*) E.t

  val sfloat : ser
  val sstring : ser
  val sbool : ser
  val schar : ser
  val si8 : ser
  val si16 : ser
  val si24 : ser
  val si32 : ser
  val si40 : ser
  val si48 : ser
  val si56 : ser
  val si64 : ser
  val si128 : ser
  val su8 : ser
  val su16 : ser
  val su24 : ser
  val su32 : ser
  val su40 : ser
  val su48 : ser
  val su56 : ser
  val su64 : ser
  val su128 : ser
  val sext : ((*v*) E.t -> (*ptr*) E.t -> (*v*ptr*) E.t) -> ser

  (* NOTE regarding _opn functions:
   * When dessser handle fieldmasks, subtypes and vectors dim will not be so
   * relevant anymore, nor could they easily be trimmed down according to the
   * fieldmask. RingBuff SER still does need it to compute the width of the
   * bitmask though. So the SER will need to be given the full representation
   * of the current fieldmask (as in CodeGen_OCaml). *)
  val tup_opn : T.mn array ->
                state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  val tup_cls : state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  val tup_sep : state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  val rec_opn : (string * T.mn) array ->
                state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  val rec_cls : state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  val rec_sep : state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  (* Takes the label as an u16: *)
  val sum_opn : (string * T.mn) array -> (*u16*) E.t ->
                state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  val sum_cls : (*u16*) E.t ->
                state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  val vec_opn : (*dim*) int -> T.mn ->
                state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  val vec_cls : state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  val vec_sep : state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  val arr_opn : T.mn -> (*u32*) E.t option ->
                state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  val arr_cls : state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  val arr_sep : state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t

  val nullable : state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  val snull : T.t ->
                state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t
  val snotnull : T.t ->
                state -> T.mn -> Path.t -> (*ptr*) E.t -> (*ptr*) E.t

  (* Sometimes, we'd like to know in advance how large a serialized value is
   * going to be. Value must have been deserialized into a heap value. *)
  type ssizer = T.mn -> Path.t -> (*value*) E.t -> E.t (*size*)
  val ssize_of_float : ssizer
  val ssize_of_string : ssizer
  val ssize_of_bool : ssizer
  val ssize_of_char : ssizer
  val ssize_of_i8 : ssizer
  val ssize_of_i16 : ssizer
  val ssize_of_i24 : ssizer
  val ssize_of_i32 : ssizer
  val ssize_of_i40 : ssizer
  val ssize_of_i48 : ssizer
  val ssize_of_i56 : ssizer
  val ssize_of_i64 : ssizer
  val ssize_of_i128 : ssizer
  val ssize_of_u8 : ssizer
  val ssize_of_u16 : ssizer
  val ssize_of_u24 : ssizer
  val ssize_of_u32 : ssizer
  val ssize_of_u40 : ssizer
  val ssize_of_u48 : ssizer
  val ssize_of_u56 : ssizer
  val ssize_of_u64 : ssizer
  val ssize_of_u128 : ssizer
  (* Specifically for the compound, excluding the size of the parts: *)
  val ssize_of_tup : ssizer
  val ssize_of_rec : ssizer
  val ssize_of_sum : ssizer
  val ssize_of_vec : ssizer
  val ssize_of_arr : ssizer
  val ssize_of_null : T.mn -> Path.t -> E.t (*size*)
  (* The size that's added to any value of this type in addition to the size
   * of its constituents: *)
  val ssize_start : ?config:config -> T.mn -> E.t (*size*)
end

(* Now we can combine a DES and a SER to create a converter from one format
 * into another (including heap values, with some trickery) *)
module DesSer (Des : DES) (Ser : SER) =
struct
  (* Most of the functions below return the src and dst pointers advanced to
   * point to the next value to read/write.
   * [mn] denotes the maybe_nullable of the current subfields, whereas
   * [mn0] denotes the maybe_nullable of the whole value. *)
  let ds ser des what transform sstate dstate mn0 path src_dst =
    E.with_sploded_pair "ds1" src_dst (fun src dst ->
      let v_src = des dstate mn0 path src in
      E.with_sploded_pair "ds2" v_src (fun v src ->
        let v = transform mn0 path v in
        let open E.Ops in
        (* dessser handle nulls itself, so that DES/SER implementations
         * do not have to care for nullability. *)
        make_pair
          (comment ("Desserialize a "^ what) src)
          (comment ("Serialize a "^ what) (ser sstate mn0 path v dst))))

  let dsfloat = ds Ser.sfloat Des.dfloat "float"
  let dsstring = ds Ser.sstring Des.dstring "string"
  let dsbool = ds Ser.sbool Des.dbool "bool"
  let dschar = ds Ser.schar Des.dchar "char"
  let dsi8 = ds Ser.si8 Des.di8 "i8"
  let dsi16 = ds Ser.si16 Des.di16 "i16"
  let dsi24 = ds Ser.si24 Des.di24 "i24"
  let dsi32 = ds Ser.si32 Des.di32 "i32"
  let dsi40 = ds Ser.si40 Des.di40 "i40"
  let dsi48 = ds Ser.si48 Des.di48 "i48"
  let dsi56 = ds Ser.si56 Des.di56 "i56"
  let dsi64 = ds Ser.si64 Des.di64 "i64"
  let dsi128 = ds Ser.si128 Des.di128 "i128"
  let dsu8 = ds Ser.su8 Des.du8 "u8"
  let dsu16 = ds Ser.su16 Des.du16 "u16"
  let dsu24 = ds Ser.su24 Des.du24 "u24"
  let dsu32 = ds Ser.su32 Des.du32 "u32"
  let dsu40 = ds Ser.su40 Des.du40 "u40"
  let dsu48 = ds Ser.su48 Des.du48 "u48"
  let dsu56 = ds Ser.su56 Des.du56 "u56"
  let dsu64 = ds Ser.su64 Des.du64 "u64"
  let dsu128 = ds Ser.su128 Des.du128 "u128"

  let dsnull t sstate dstate mn0 path src dst =
    let open E.Ops in
    make_pair
      (comment "Desserialize NULL" (Des.dnull t dstate mn0 path src))
      (comment "Serialize NULL" (Ser.snull t sstate mn0 path dst))

  let dsnotnull t sstate dstate mn0 path src dst =
    let open E.Ops in
    make_pair
      (comment "Desserialize NonNull" (Des.dnotnull t dstate mn0 path src))
      (comment "Serialize NonNull" (Ser.snotnull t sstate mn0 path dst))

  (* transform is applied to leaf values only, as compound values are not
   * reified during a dessser operation *)
  let rec dstup mns transform sstate dstate mn0 path src_dst =
    let open E.Ops in
    let src_dst = comment "Convert a Tuple"
      (E.with_sploded_pair "dstup1" src_dst (fun src dst ->
        make_pair
          (Des.tup_opn mns dstate mn0 path src)
          (Ser.tup_opn mns sstate mn0 path dst))) in
    let src_dst =
      BatArray.fold_lefti (fun src_dst i _mn ->
        comment ("Convert tuple field "^ Stdlib.string_of_int i)
          (let subpath = Path.(append (CompTime i) path) in
          if i = 0 then
            desser_ transform sstate dstate mn0 subpath src_dst
          else
            let src_dst =
              E.with_sploded_pair "dstup2" src_dst (fun src dst ->
                make_pair
                  (Des.tup_sep dstate mn0 path src)
                  (Ser.tup_sep sstate mn0 path dst)) in
            desser_ transform sstate dstate mn0 subpath src_dst)
      ) src_dst mns in
    E.with_sploded_pair "dstup3" src_dst (fun src dst ->
      make_pair
        (Des.tup_cls dstate mn0 path src)
        (Ser.tup_cls sstate mn0 path dst))

  and dsrec mns transform sstate dstate mn0 path src_dst =
    let open E.Ops in
    let src_dst =
      E.with_sploded_pair "dsrec1" src_dst (fun src dst ->
        make_pair
          (Des.rec_opn mns dstate mn0 path src)
          (Ser.rec_opn mns sstate mn0 path dst)) in
    let src_dst =
      BatArray.fold_lefti (fun src_dst i (name, _mn) ->
          comment ("Convert record field "^ name)
            (let subpath = Path.(append (CompTime i) path) in
            if i = 0 then
              desser_ transform sstate dstate mn0 subpath src_dst
            else
              let src_dst =
                E.with_sploded_pair "dsrec2" src_dst (fun src dst ->
                  make_pair
                    (Des.rec_sep dstate mn0 path src)
                    (Ser.rec_sep sstate mn0 path dst)) in
              desser_ transform sstate dstate mn0 subpath src_dst)
      ) src_dst mns in
    let src_dst = comment "Convert a Record" src_dst in
    E.with_sploded_pair "dsrec3" src_dst (fun src dst ->
      make_pair
        (Des.rec_cls dstate mn0 path src)
        (Ser.rec_cls sstate mn0 path dst))

  and dssum mns transform sstate dstate mn0 path src_dst =
    let open E.Ops in
    let max_lbl = Array.length mns - 1 in
    E.with_sploded_pair "dssum1" src_dst (fun src dst ->
      let cstr_src = Des.sum_opn mns dstate mn0 path src in
      let src_dst =
        E.with_sploded_pair "dssum2" cstr_src (fun cstr src ->
          let dst = Ser.sum_opn mns cstr sstate mn0 path dst in
          let src_dst = make_pair src dst in
          let rec choose_cstr i =
            let subpath = Path.(append (CompTime i) path) in
            if i >= max_lbl then
              seq [
                assert_ (eq cstr (u16 (Uint16.of_int max_lbl))) ;
                desser_ transform sstate dstate mn0 subpath src_dst ]
            else
              if_ (eq (u16 (Uint16.of_int i)) cstr)
                ~then_:(desser_ transform sstate dstate mn0 subpath src_dst)
                ~else_:(choose_cstr (i + 1)) in
          choose_cstr 0) in
      let cstr = first cstr_src in
      E.with_sploded_pair "dssum3" src_dst (fun src dst ->
        make_pair
          (Des.sum_cls cstr dstate mn0 path src)
          (Ser.sum_cls cstr sstate mn0 path dst)))

  and dsvec dim mn transform sstate dstate mn0 path src_dst =
    let open E.Ops in
    let src_dst =
      E.with_sploded_pair "dsvec1" src_dst (fun src dst ->
        make_pair
          (Des.vec_opn dim mn dstate mn0 path src)
          (Ser.vec_opn dim mn sstate mn0 path dst)) in
    let src_dst =
      let_ ~name:"src_dst_ref" (make_ref src_dst) (fun src_dst_ref ->
        let src_dst = get_ref src_dst_ref in
        seq [
          StdLib.repeat ~from:(i32 0l) ~to_:(i32_of_int dim) (fun n ->
            (comment "Convert vector item"
              (let subpath = Path.(append (RunTime (to_u32 n)) path) in
              let src_dst =
                if_ (eq n (i32 0l))
                  ~then_:src_dst
                  ~else_:(
                    E.with_sploded_pair "dsvec2" src_dst (fun src dst ->
                      make_pair
                        (Des.vec_sep dstate mn0 subpath src)
                        (Ser.vec_sep sstate mn0 subpath dst))) in
              desser_ transform sstate dstate mn0 subpath src_dst |>
              set_ref src_dst_ref))) ;
          src_dst ])
    in
    E.with_sploded_pair "dsvec3" src_dst (fun src dst ->
      make_pair
        (Des.vec_cls dstate mn0 path src)
        (Ser.vec_cls sstate mn0 path dst))

  and dsarr mn transform sstate dstate mn0 path src_dst =
    let open E.Ops in
    (* Pretend we visit only the index 0, which is enough to determine
     * subtypes: *)
    comment "Convert an array "
      (E.with_sploded_pair "dsarr1" src_dst (fun src dst ->
        (* FIXME: for some deserializers (such as SExpr) it's not easy to
         * know the arr length in advance. For those, it would be better
         * to call a distinct 'is-end-of-arr' function returning a bool and
         * read until this returns true. We could simply have both, and
         * here we would repeat_while ~cond:(n<count && not is_end_of_arr),
         * then SEpxr would merely return a very large number of entries
         * (better than to return a single condition in arr_opn for non
         * functional backends such as, eventually, C?) *)
        match Des.arr_opn dstate with
        | KnownSize arr_opn ->
            let src_dst =
              let dim_src = arr_opn mn mn0 path src in
              E.with_sploded_pair "dsarr2" dim_src (fun dim src ->
                let dst = Ser.arr_opn mn (Some dim) sstate mn0 path dst in
                let src_dst_ref = make_ref (make_pair src dst) in
                let_ ~name:"src_dst_ref" src_dst_ref (fun src_dst_ref ->
                  let src_dst = get_ref src_dst_ref in
                  seq [
                    StdLib.repeat ~from:(i32 0l) ~to_:(to_i32 dim) (fun n ->
                      (comment "Convert a arr item"
                        (let subpath = Path.(append (RunTime (to_u32 n)) path) in
                        let src_dst =
                          if_ (eq n (i32 0l))
                            ~then_:src_dst
                            ~else_:(
                              E.with_sploded_pair "dsarr3" src_dst
                                (fun psrc pdst ->
                                make_pair
                                  (Des.arr_sep dstate mn0 subpath psrc)
                                  (Ser.arr_sep sstate mn0 subpath pdst))
                            ) in
                        desser_ transform sstate dstate mn0 subpath src_dst |>
                        set_ref src_dst_ref))) ;
                    src_dst ])) in
            let_pair ~n1:"src" ~n2:"dst" src_dst (fun src dst ->
              make_pair
                (Des.arr_cls dstate mn0 path src)
                (Ser.arr_cls sstate mn0 path dst))
        | UnknownSize (arr_opn, end_of_arr) ->
            let src = arr_opn mn mn0 path src in
            let dst = Ser.arr_opn mn None sstate mn0 path dst in
            let src_dst_ref = make_ref (make_pair src dst) in
            let_ ~name:"src_dst_ref" src_dst_ref (fun src_dst_ref ->
              let src = first (get_ref src_dst_ref) in
              let dst = secnd (get_ref src_dst_ref) in
              let_ ~name:"n_ref" (make_ref (u32_of_int 0)) (fun n_ref ->
                let n = get_ref n_ref in
                seq [
                  while_
                    (comment "Test end of arr"
                      (not_ (end_of_arr mn0 path src)))
                    (comment "Convert an array item"
                      (let subpath = Path.(append (RunTime n) path) in
                      seq [
                        if_ (eq n (u32_of_int 0))
                          ~then_:nop
                          ~else_:(
                            set_ref src_dst_ref
                              (make_pair
                                (Des.arr_sep dstate mn0 subpath src)
                                (Ser.arr_sep sstate mn0 subpath dst))) ;
                        set_ref n_ref (add n (u32_of_int 1)) ;
                        set_ref src_dst_ref
                          (desser_ transform sstate dstate mn0 subpath
                                   (get_ref src_dst_ref)) ])) ;
                  make_pair
                    (Des.arr_cls dstate mn0 path src)
                    (Ser.arr_cls sstate mn0 path dst) ]))))

  (* TODO: Ext *)
  and desser_value = function
    | T.This _ -> assert false (* Because of Path.type_of_path *)
    | Void ->
        fun _transform _sstate _dstate _mn0 _path src_dst -> src_dst
    | Float -> dsfloat
    | String -> dsstring
    | Bool -> dsbool
    | Char -> dschar
    | I8 -> dsi8
    | I16 -> dsi16
    | I24 -> dsi24
    | I32 -> dsi32
    | I40 -> dsi40
    | I48 -> dsi48
    | I56 -> dsi56
    | I64 -> dsi64
    | I128 -> dsi128
    | U8 -> dsu8
    | U16 -> dsu16
    | U24 -> dsu24
    | U32 -> dsu32
    | U40 -> dsu40
    | U48 -> dsu48
    | U56 -> dsu56
    | U64 -> dsu64
    | U128 -> dsu128
    | Usr vt -> desser_value vt.def
    | Tup mns -> dstup mns
    | Rec mns -> dsrec mns
    | Sum mns -> dssum mns
    | Vec (dim, mn) -> dsvec dim mn
    | Arr mn
    (* Sets and Lsts are serialized like arrs (the last update is thus lost). *)
    | Set (Simple, mn) | T.Lst mn -> dsarr mn
    | Set _ -> todo "des/ser for non simple sets"
    | Map _ -> assert false (* No value of map type *)
    | _ -> invalid_arg "desser_value"

  and desser_ transform sstate dstate mn0 path src_dst =
    let open E.Ops in
    let mn = Path.type_of_path mn0 path in
    if mn.T.nullable then (
      E.with_sploded_pair "desser_" src_dst (fun src dst ->
        (* Des can use [is_null] to prepare for a nullable, but Ser might also
         * have some work to do: *)
        let dst = Ser.nullable sstate mn0 path dst in
        (* XXX WARNING XXX
         * if any of dnull/snull/snotnull/etc update the state, they will
         * do so in both branches of this alternative. *)
        if_ (Des.is_null dstate mn0 path src)
          ~then_:(dsnull mn.typ sstate dstate mn0 path src dst)
          ~else_:(dsnotnull mn.typ sstate dstate mn0 path src dst |>
                  desser_value mn.typ transform sstate dstate mn0 path))
    ) else (
      desser_value mn.typ transform sstate dstate mn0 path src_dst
    )

  let desser ?ser_config ?des_config mn0 ?transform src dst =
    let no_transform _mn0 _path v = v in
    let transform = transform |? no_transform in
    let open E.Ops in
    let sstate, dst = Ser.start mn0 ?config:ser_config dst
    and dstate, src = Des.start mn0 ?config:des_config src in
    let src_dst = make_pair src dst in
    let src_dst = desser_ transform sstate dstate mn0 [] src_dst in
    E.with_sploded_pair "desser" src_dst (fun src dst ->
      make_pair
        (Des.stop dstate src)
        (Ser.stop sstate dst))
end

(*
 * Compilation units are sets of definitions and declarations of external
 * values.
 *)

(*
 * Now let's move on to code generators.
 *
 * Eventually, a compilation unit is turned into an actual source file.
 * Unused identifiers may not be included unless they are non-anonymous
 * functions.
 *)

type link = Object | SharedObject | Executable

module type BACKEND =
sig
  val id : backend_id
  val print_definitions : (U.t, 'b) IO.printer
  val print_declarations : (U.t, 'b) IO.printer
  val print_comment : 'b IO.output -> ('a, 'b IO.output, unit) format -> 'a
  val valid_source_name : string -> string
  val preferred_def_extension : string
  val preferred_decl_extension : string
  val preferred_comp_extension : link -> string
  val compile_cmd : ?dev_mode:bool -> ?extra_search_paths:string list -> ?optim:int -> link:link -> string -> string -> string
  val type_identifier : Printer.t -> T.t -> string
  val type_identifier_mn : Printer.t -> T.mn -> string
end
