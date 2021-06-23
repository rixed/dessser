open Batteries
open Stdint
open DessserTools
module T = DessserTypes
module E = DessserExpressions
module Printer = DessserPrinter
module Path = DessserPath
module StdLib = DessserStdLib

(* Used by deserializers to "open" lists: *)
type list_opener =
  (* When a list size is known from the beginning, implement this that
   * returns both the list size and the new src pointer: *)
  | KnownSize of (T.maybe_nullable -> Path.t -> T.maybe_nullable -> E.env -> (*ptr*) E.t -> (* (u32 * ptr) *) E.t)
  (* Whereas when the list size is not known beforehand, rather implement
   * this pair of functions, one to parse the list header and return the new
   * src pointer and one that will be called before any new token and must
   * return true if the list is finished: *)
  | UnknownSize of
      (T.maybe_nullable -> Path.t -> T.maybe_nullable -> E.env -> (*ptr*) E.t -> (*ptr*) E.t) *
      (T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*bool*) E.t)

(* Given the above we can built interesting expressions.
 * A DES(serializer) is a module that implements a particular serialization
 * format in such a way that it provides simple expressions for the basic
 * types that can then be assembled to build either a deserializer from
 * DataPtr to a heap value, or a converter between two formats. *)
module type DES =
sig
  val id : T.encoding_id

  (* DES and SER can have some configurable parameters: *)
  type config

  (* No need for a backend (BE) since we merely compute expressions *)
  (* RW state passed to every deserialization operations *)
  type state
  (* Wraps a DataPtr into whatever the DES needs *)
  val ptr : T.maybe_nullable -> T.t

  val start : ?config:config -> T.maybe_nullable -> E.env -> (*dataptr*) E.t -> state * (*ptr*) E.t
  val stop : state -> E.env -> (*ptr*) E.t -> (*ptr*) E.t

  (* A basic value deserializer takes a state, an expression
   * yielding a pointer (either a CodePtr pointing at a byte stream or a
   * ValuePtr pointing at a heap value of type ['b]), and returns two
   * expressions: one yielding the advanced pointer (of the exact same type) and
   * one yielding the value that's been deserialized from the given location.
   * The [maybe_nullable] and [path] values are in the fully fledged global
   * type and therefore must be used cautiously! *)
  (* FIXME: make this type "private": *)
  type des = state -> T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*v*ptr*) E.t

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

  (* Paths passed to opn/cls/sep functions are the path of the compound structure
   * itself *)

  (* Get rid of the _seps (in SExpr, use a state to add a separator before any
   * value instead). That would make the code generated for dessser significantly
   * simpler, and even more so when runtime fieldmasks enter the stage! *)
  val tup_opn : state -> T.maybe_nullable -> Path.t -> T.maybe_nullable array -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  val tup_cls : state -> T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  val tup_sep : state -> T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  val rec_opn : state -> T.maybe_nullable -> Path.t -> (string * T.maybe_nullable) array -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  val rec_cls : state -> T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  val rec_sep : state -> T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  (* Returns the label as an u16 and the new pointer: *)
  val sum_opn : state -> T.maybe_nullable -> Path.t -> (string * T.maybe_nullable) array -> E.env -> (*ptr*) E.t -> (* u16*ptr *) E.t
  val sum_cls : state -> T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  val vec_opn : state -> T.maybe_nullable -> Path.t -> (*dim*) int -> T.maybe_nullable -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  val vec_cls : state -> T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  val vec_sep : state -> T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  val list_opn : state -> list_opener
  val list_cls : state -> T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  val list_sep : state -> T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*ptr*) E.t

  val is_null : state -> T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*bool*) E.t
  val dnull : T.value -> state -> T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  val dnotnull : T.value -> state -> T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
end

module type SER =
sig
  val id : T.encoding_id

  (* DES and SER can have some configurable parameters: *)
  type config

  (* RW state passed to every serialization operations *)
  type state
  (* Wraps a DataPtr into whatever the SER needs *)
  val ptr : T.maybe_nullable -> T.t

  val start : ?config:config -> T.maybe_nullable -> E.env -> (*dataptr*) E.t -> state * (*ptr*) E.t
  val stop : state -> E.env -> (*ptr*) E.t -> (*ptr*) E.t

  (* FIXME: make this type "private": *)
  type ser = state -> T.maybe_nullable -> Path.t -> E.env -> (*v*) E.t -> (*ptr*) E.t -> (*ptr*) E.t

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

  (* NOTE regarding _opn functions:
   * When dessser handle fieldmasks, subtypes and vectors dim will not be so
   * relevant anymore, nor could they easily be trimmed down according to the
   * fieldmask. RingBuff SER still does need it to compute the width of the
   * bitmask though. So the SER will need to be given the full representation
   * of the current fieldmask (as in CodeGen_OCaml). *)
  val tup_opn : state -> T.maybe_nullable -> Path.t -> T.maybe_nullable array -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  val tup_cls : state -> T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  val tup_sep : state -> T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  val rec_opn : state -> T.maybe_nullable -> Path.t -> (string * T.maybe_nullable) array -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  val rec_cls : state -> T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  val rec_sep : state -> T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  (* Takes the label as an u16: *)
  val sum_opn : state -> T.maybe_nullable -> Path.t -> (string * T.maybe_nullable) array -> E.env -> (*u16*) E.t -> (*ptr*) E.t -> (*ptr*) E.t
  val sum_cls : state -> T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  val vec_opn : state -> T.maybe_nullable -> Path.t -> (*dim*) int -> T.maybe_nullable -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  val vec_cls : state -> T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  val vec_sep : state -> T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  val list_opn : state -> T.maybe_nullable -> Path.t -> T.maybe_nullable -> (*u32*) E.t option -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  val list_cls : state -> T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  val list_sep : state -> T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*ptr*) E.t

  val nullable : state -> T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  val snull : T.value -> state -> T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*ptr*) E.t
  val snotnull : T.value -> state -> T.maybe_nullable -> Path.t -> E.env -> (*ptr*) E.t -> (*ptr*) E.t

  (* Sometimes, we'd like to know in advance how large a serialized value is
   * going to be. Value must have been deserialized into a heap value. *)
  type ssizer = T.maybe_nullable -> Path.t -> E.env -> (*value*) E.t -> E.t (*size*)
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
  val ssize_of_list : ssizer
  val ssize_of_null : T.maybe_nullable -> Path.t -> E.t (*size*)
  (* The size that's added to any value of this type in addition to the size
   * of its constituents: *)
  val ssize_start : ?config:config -> T.maybe_nullable -> E.t (*size*)
end

(* Now we can combine a DES and a SER to create a converter from one format
 * into another (including heap values, with some trickery) *)
module DesSer (Des : DES) (Ser : SER) =
struct
  (* Most of the functions below return the src and dst pointers advanced to
   * point to the next value to read/write.
   * [mn] denotes the maybe_nullable of the current subfields, whereas
   * [mn0] denotes the maybe_nullable of the whole value. *)
  let ds ser des what transform sstate dstate mn0 path l src_dst =
    E.with_sploded_pair ~l "ds1" src_dst (fun l src dst ->
      let v_src = des dstate mn0 path l src in
      E.with_sploded_pair ~l "ds2" v_src (fun l v src ->
        let v = transform mn0 path l v in
        let open E.Ops in
        (* dessser handle nulls itself, so that DES/SER implementations
         * do not have to care for nullability. *)
        make_pair
          (comment ("Desserialize a "^ what) src)
          (comment ("Serialize a "^ what) (ser sstate mn0 path l v dst))))

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

  let dsnull t sstate dstate mn0 path l src dst =
    let open E.Ops in
    make_pair
      (comment "Desserialize NULL" (Des.dnull t dstate mn0 path l src))
      (comment "Serialize NULL" (Ser.snull t sstate mn0 path l dst))

  let dsnotnull t sstate dstate mn0 path l src dst =
    let open E.Ops in
    make_pair
      (comment "Desserialize NonNull" (Des.dnotnull t dstate mn0 path l src))
      (comment "Serialize NonNull" (Ser.snotnull t sstate mn0 path l dst))

  (* transform is applied to leaf values only, as compound values are not
   * reified during a dessser operation *)
  let rec dstup mns transform sstate dstate mn0 path l src_dst =
    let open E.Ops in
    let src_dst = comment "Convert a Tuple"
      (E.with_sploded_pair ~l "dstup1" src_dst (fun l src dst ->
        make_pair
          (Des.tup_opn dstate mn0 path mns l src)
          (Ser.tup_opn sstate mn0 path mns l dst))) in
    let src_dst =
      BatArray.fold_lefti (fun src_dst i _mn ->
        comment ("Convert tuple field "^ Stdlib.string_of_int i)
          (let subpath = Path.(append (CompTime i) path) in
          if i = 0 then
            desser_ transform sstate dstate mn0 subpath l src_dst
          else
            let src_dst =
              E.with_sploded_pair ~l "dstup2" src_dst (fun l src dst ->
                make_pair
                  (Des.tup_sep dstate mn0 path l src)
                  (Ser.tup_sep sstate mn0 path l dst)) in
            desser_ transform sstate dstate mn0 subpath l src_dst)
      ) src_dst mns in
    E.with_sploded_pair ~l "dstup3" src_dst (fun l src dst ->
      make_pair
        (Des.tup_cls dstate mn0 path l src)
        (Ser.tup_cls sstate mn0 path l dst))

  and dsrec mns transform sstate dstate mn0 path l src_dst =
    let open E.Ops in
    let src_dst =
      E.with_sploded_pair ~l "dsrec1" src_dst (fun l src dst ->
        make_pair
          (Des.rec_opn dstate mn0 path mns l src)
          (Ser.rec_opn sstate mn0 path mns l dst)) in
    let src_dst =
      BatArray.fold_lefti (fun src_dst i (name, _mn) ->
          comment ("Convert record field "^ name)
            (let subpath = Path.(append (CompTime i) path) in
            if i = 0 then
              desser_ transform sstate dstate mn0 subpath l src_dst
            else
              let src_dst =
                E.with_sploded_pair ~l "dsrec2" src_dst (fun l src dst ->
                  make_pair
                    (Des.rec_sep dstate mn0 path l src)
                    (Ser.rec_sep sstate mn0 path l dst)) in
              desser_ transform sstate dstate mn0 subpath l src_dst)
      ) src_dst mns in
    let src_dst = comment "Convert a Record" src_dst in
    E.with_sploded_pair ~l "dsrec3" src_dst (fun l src dst ->
      make_pair
        (Des.rec_cls dstate mn0 path l src)
        (Ser.rec_cls sstate mn0 path l dst))

  and dssum mns transform sstate dstate mn0 path l src_dst =
    let open E.Ops in
    let max_lbl = Array.length mns - 1 in
    E.with_sploded_pair ~l "dssum1" src_dst (fun l src dst ->
      let cstr_src = Des.sum_opn dstate mn0 path mns l src in
      let src_dst =
        E.with_sploded_pair ~l "dssum2" cstr_src (fun l cstr src ->
          let dst = Ser.sum_opn sstate mn0 path mns l cstr dst in
          let src_dst = make_pair src dst in
          let rec choose_cstr i =
            let subpath = Path.(append (CompTime i) path) in
            if i >= max_lbl then
              seq [
                assert_ (eq cstr (u16 (Uint16.of_int max_lbl))) ;
                desser_ transform sstate dstate mn0 subpath l src_dst ]
            else
              if_ (eq (u16 (Uint16.of_int i)) cstr)
                ~then_:(desser_ transform sstate dstate mn0 subpath l src_dst)
                ~else_:(choose_cstr (i + 1)) in
          choose_cstr 0) in
      E.with_sploded_pair ~l "dssum3" src_dst (fun l src dst ->
        make_pair
          (Des.sum_cls dstate mn0 path l src)
          (Ser.sum_cls sstate mn0 path l dst)))

  and dsvec dim mn transform sstate dstate mn0 path l src_dst =
    let open E.Ops in
    let src_dst =
      E.with_sploded_pair ~l "dsvec1" src_dst (fun l src dst ->
        make_pair
          (Des.vec_opn dstate mn0 path dim mn l src)
          (Ser.vec_opn sstate mn0 path dim mn l dst)) in
    let src_dst =
      StdLib.repeat ~l
        ~init:src_dst
        ~from:(i32 0l) ~to_:(i32 (Int32.of_int dim))
        ~body:(fun l n src_dst ->
          (comment "Convert vector item"
            (let subpath = Path.(append (RunTime (to_u32 n)) path) in
            let src_dst =
              if_ (eq n (i32 0l))
                ~then_:src_dst
                ~else_:(
                  E.with_sploded_pair ~l "dsvec2" src_dst (fun l src dst ->
                    make_pair
                      (Des.vec_sep dstate mn0 subpath l src)
                      (Ser.vec_sep sstate mn0 subpath l dst))) in
            desser_ transform sstate dstate mn0 subpath l src_dst)))
    in
    E.with_sploded_pair ~l "dsvec3" src_dst (fun l src dst ->
      make_pair
        (Des.vec_cls dstate mn0 path l src)
        (Ser.vec_cls sstate mn0 path l dst))

  and dslist mn transform sstate dstate mn0 path l src_dst =
    let open E.Ops in
    (* Pretend we visit only the index 0, which is enough to determine
     * subtypes: *)
    comment "Convert a List"
      (E.with_sploded_pair ~l "dslist1" src_dst (fun l src dst ->
        (* FIXME: for some deserializers (such as SExpr) it's not easy to
         * know the list length in advance. For those, it would be better
         * to call a distinct 'end-of-list' function returning a bool and
         * read until this returns true. We could simply have both, and
         * here we would repeat_while ~cond:(n<count && not end_of_list),
         * then SEpxr would merely return a very large number of entries
         * (better than to return a single condition in list_opn for non
         * functional backends such as, eventually, C?) *)
        match Des.list_opn dstate with
        | KnownSize list_opn ->
            let src_dst =
              let dim_src = list_opn mn0 path mn l src in
              E.with_sploded_pair ~l "dslist2" dim_src (fun l dim src ->
                let dst = Ser.list_opn sstate mn0 path mn (Some dim) l dst in
                StdLib.repeat ~l
                  ~init:(make_pair src dst)
                  ~from:(i32 0l) ~to_:(to_i32 dim)
                  ~body:(fun l n src_dst ->
                    (comment "Convert a list item"
                      (let subpath = Path.(append (RunTime (to_u32 n)) path) in
                      let src_dst =
                        if_ (eq n (i32 0l))
                          ~then_:src_dst
                          ~else_:(
                            E.with_sploded_pair ~l "dslist3" src_dst (fun l psrc pdst ->
                              make_pair
                                (Des.list_sep dstate mn0 subpath l psrc)
                                (Ser.list_sep sstate mn0 subpath l pdst))
                          ) in
                      desser_ transform sstate dstate mn0 subpath l src_dst)))) in
            let_pair ~n1:"src" ~n2:"dst" ~l src_dst (fun l src dst ->
              make_pair
                (Des.list_cls dstate mn0 path l src)
                (Ser.list_cls sstate mn0 path l dst))
          | UnknownSize (list_opn, end_of_list) ->
              let src = list_opn mn0 path mn l src in
              let dst = Ser.list_opn sstate mn0 path mn None l dst in
              let src_dst_ref = make_ref (make_pair src dst) in
              let_ ~name:"src_dst_ref" ~l src_dst_ref (fun l src_dst_ref ->
                let src = first (get_ref src_dst_ref) in
                let dst = secnd (get_ref src_dst_ref) in
                let_ ~name:"n_ref" ~l (make_ref (u32_of_int 0)) (fun l n_ref ->
                  let n = get_ref n_ref in
                  seq [
                    while_
                      (comment "Test end of list"
                        (not_ (end_of_list mn0 path l src)))
                      (comment "Convert a list item"
                        (let subpath = Path.(append (RunTime n) path) in
                        seq [
                          if_ (eq n (u32_of_int 0))
                            ~then_:nop
                            ~else_:(
                              set_ref src_dst_ref
                                (make_pair
                                  (Des.list_sep dstate mn0 subpath l src)
                                  (Ser.list_sep sstate mn0 subpath l dst))) ;
                          set_ref n_ref (add n (u32_of_int 1)) ;
                          set_ref src_dst_ref
                            (desser_ transform sstate dstate mn0 subpath l
                                     (get_ref src_dst_ref)) ])) ;
                    make_pair
                      (Des.list_cls dstate mn0 path l src)
                      (Ser.list_cls sstate mn0 path l dst) ]))))

  and desser_value = function
    | T.Unknown | T.Ext _ -> invalid_arg "desser_value"
    | T.This -> assert false (* Because of Path.type_of_path *)
    | T.Base Unit -> fun _transform _sstate _dstate _mn0 _path _l src_dst -> src_dst
    | T.Base Float -> dsfloat
    | T.Base String -> dsstring
    | T.Base Bool -> dsbool
    | T.Base Char -> dschar
    | T.Base I8 -> dsi8
    | T.Base I16 -> dsi16
    | T.Base I24 -> dsi24
    | T.Base I32 -> dsi32
    | T.Base I40 -> dsi40
    | T.Base I48 -> dsi48
    | T.Base I56 -> dsi56
    | T.Base I64 -> dsi64
    | T.Base I128 -> dsi128
    | T.Base U8 -> dsu8
    | T.Base U16 -> dsu16
    | T.Base U24 -> dsu24
    | T.Base U32 -> dsu32
    | T.Base U40 -> dsu40
    | T.Base U48 -> dsu48
    | T.Base U56 -> dsu56
    | T.Base U64 -> dsu64
    | T.Base U128 -> dsu128
    | T.Usr vt -> desser_value vt.def
    | T.Tup mns -> dstup mns
    | T.Rec mns -> dsrec mns
    | T.Sum mns -> dssum mns
    | T.Vec (dim, mn) -> dsvec dim mn
    | T.Lst mn -> dslist mn
    (* Sets are serialized like lists (the last update is thus lost). *)
    | T.Set (Simple, mn) -> dslist mn
    | T.Set _ -> todo "des/ser for non simple sets"
    | T.Map _ -> assert false (* No value of map type *)

  and desser_ transform sstate dstate mn0 path l src_dst =
    let open E.Ops in
    let mn = Path.type_of_path mn0 path in
    if mn.nullable then (
      E.with_sploded_pair ~l "desser_" src_dst (fun l src dst ->
        (* Des can use [is_null] to prepare for a nullable, but Ser might also
         * have some work to do: *)
        let dst = Ser.nullable sstate mn0 path l dst in
        (* XXX WARNING XXX
         * if any of dnull/snull/snotnull/etc update the state, they will
         * do so in both branches of this alternative. *)
        if_ (Des.is_null dstate mn0 path l src)
          ~then_:(dsnull mn.vtyp sstate dstate mn0 path l src dst)
          ~else_:(dsnotnull mn.vtyp sstate dstate mn0 path l src dst |>
                  desser_value mn.vtyp transform sstate dstate mn0 path l))
    ) else (
      desser_value mn.vtyp transform sstate dstate mn0 path l src_dst
    )

  let desser ?ser_config ?des_config mn0 ?transform l src dst =
    let no_transform _mn0 _path _l v = v in
    let transform = transform |? no_transform in
    let open E.Ops in
    let sstate, dst = Ser.start mn0 ?config:ser_config l dst
    and dstate, src = Des.start mn0 ?config:des_config l src in
    let src_dst = make_pair src dst in
    let src_dst = desser_ transform sstate dstate mn0 [] l src_dst in
    E.with_sploded_pair ~l "desser" src_dst (fun l src dst ->
      make_pair
        (Des.stop dstate l src)
        (Ser.stop sstate l dst))
end

(*
 * Compilation units are sets of definitions and declarations of external
 * values.
 *)

module U = DessserCompilationUnit

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
  val id : T.backend_id
  val print_definitions : (U.t, 'b) IO.printer
  val print_declarations : (U.t, 'b) IO.printer
  val print_comment : 'b IO.output -> ('a, 'b IO.output, unit) format -> 'a
  val valid_source_name : string -> string
  val preferred_def_extension : string
  val preferred_decl_extension : string
  val preferred_comp_extension : link -> string
  val compile_cmd : ?dev_mode:bool -> ?extra_search_paths:string list -> ?optim:int -> link:link -> string -> string -> string
  val type_identifier : Printer.t -> T.t -> string
end
