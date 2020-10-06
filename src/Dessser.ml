open Batteries
open Stdint
open DessserTools
module T = DessserTypes
module E = DessserExpressions

(* Used by deserializers to "open" lists: *)
type 'a list_opener =
  (* When a list size is known from the beginning, implement this that
   * returns both the list size and the new src pointer: *)
  | KnownSize of ('a -> T.maybe_nullable -> T.path -> T.maybe_nullable -> (*ptr*) E.t -> (* (u32 * ptr) *) E.t)
  (* Whereas when the list size is not known beforehand, rather implement
   * this pair of functions, one to parse the ilst header and return the new
   * src pointer and one that will be called before any new token and must
   * return true if the list is finished: *)
  | UnknownSize of
      ('a -> T.maybe_nullable -> T.path -> T.maybe_nullable -> (*ptr*) E.t -> (*ptr*) E.t) *
      ('a -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*bool*) E.t)

(* Given the above we can built interesting expressions.
 * A DES(serializer) is a module that implements a particular serialization
 * format in such a way that it provides simple expressions for the basic
 * types that can then be assembled to build either a deserializer from
 * DataPtr to a heap value, or a converter between two formats. *)
module type DES =
sig
  (* No need for a backend (BE) since we merely compute expressions *)
  (* RW state passed to every deserialization operations *)
  type state
  val ptr : T.maybe_nullable -> T.t (* either dataptr or valueptr, or whatever really  *)

  val start : T.maybe_nullable -> (*dataptr*) E.t -> state * (*ptr*) E.t
  val stop : state -> (*ptr?*) E.t -> (*ptr*) E.t

  (* A basic value deserializer takes a state, an expression
   * yielding a pointer (either a CodePtr pointing at a byte stream or a
   * ValuePtr pointing at a heap value of type ['b]), and returns two
   * expressions: one yielding the advanced pointer (of the exact same type) and
   * one yielding the value that's been deserialized from the given location.
   * The [maybe_nullable] and [path] values are in the fully fledged global
   * type and therefore must be used cautiously! *)
  (* FIXME: make this type "private": *)
  type des = state -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*v*ptr*) E.t

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

  (* TODO: not sure the _sep function need all the extra parameters that they
   * better not use (esp the index, that's wrong!)
   * Get rid of the _seps (in SExpr, use a state to add a separator before any
   * value instead). That would make the code generated for dessser significantly
   * simpler, and even more so when runtime fieldmasks enter the stage! *)
  val tup_opn : state -> T.maybe_nullable -> T.path -> T.maybe_nullable array -> (*ptr*) E.t -> (*ptr*) E.t
  val tup_cls : state -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*ptr*) E.t
  val tup_sep : int (* before *) -> state -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*ptr*) E.t
  val rec_opn : state -> T.maybe_nullable -> T.path -> (string * T.maybe_nullable) array -> (*ptr*) E.t -> (*ptr*) E.t
  val rec_cls : state -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*ptr*) E.t
  val rec_sep : string (* before *) -> state -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*ptr*) E.t
  (* Returns the label as an u16 and the new pointer: *)
  val sum_opn : state -> T.maybe_nullable -> T.path -> (string * T.maybe_nullable) array -> (*ptr*) E.t -> (* u16*ptr *) E.t
  val sum_cls : state -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*ptr*) E.t
  val vec_opn : state -> T.maybe_nullable -> T.path -> (*dim*) int -> T.maybe_nullable -> (*ptr*) E.t -> (*ptr*) E.t
  val vec_cls : state -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*ptr*) E.t
  val vec_sep : int (* before *) -> state -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*ptr*) E.t
  val list_opn : state list_opener
  val list_cls : state -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*ptr*) E.t
  val list_sep : state -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*ptr*) E.t

  val is_null : state -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*bool*) E.t
  val dnull : T.value_type -> state -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*ptr*) E.t
  val dnotnull : T.value_type -> state -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*ptr*) E.t
end

(* Same goes for SER(rializers), with the addition that it is also possible to
 * "serialize" into a heap value instead of a data stream:
 * (note: "ssize" stands for "serialized size") *)
type ssize = ConstSize of int | DynSize of (*size*) E.t

module type SER =
sig
  (* RW state passed to every serialization operations *)
  type state
  val ptr : T.maybe_nullable -> T.t (* either dataptr or valueptr *)

  val start : T.maybe_nullable -> (*dataptr*) E.t -> state * (*ptr*) E.t
  val stop : state -> (*ptr*) E.t -> (*ptr*) E.t

  (* FIXME: make this type "private": *)
  type ser = state -> T.maybe_nullable -> T.path -> (*v*) E.t -> (*ptr*) E.t -> (*ptr*) E.t

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
  val tup_opn : state -> T.maybe_nullable -> T.path -> T.maybe_nullable array -> (*ptr*) E.t -> (*ptr*) E.t
  val tup_cls : state -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*ptr*) E.t
  val tup_sep : int (* before *) -> state -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*ptr*) E.t
  val rec_opn : state -> T.maybe_nullable -> T.path -> (string * T.maybe_nullable) array -> (*ptr*) E.t -> (*ptr*) E.t
  val rec_cls : state -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*ptr*) E.t
  val rec_sep : string (* before *) -> state -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*ptr*) E.t
  (* Takes the label as an u16: *)
  val sum_opn : state -> T.maybe_nullable -> T.path -> (string * T.maybe_nullable) array -> (*u16*) E.t -> (*ptr*) E.t -> (*ptr*) E.t
  val sum_cls : state -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*ptr*) E.t
  val vec_opn : state -> T.maybe_nullable -> T.path -> (*dim*) int -> T.maybe_nullable -> (*ptr*) E.t -> (*ptr*) E.t
  val vec_cls : state -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*ptr*) E.t
  val vec_sep : int (* before *) -> state -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*ptr*) E.t
  val list_opn : state -> T.maybe_nullable -> T.path -> T.maybe_nullable -> (*u32*) E.t option -> (*ptr*) E.t -> (*ptr*) E.t
  val list_cls : state -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*ptr*) E.t
  val list_sep : state -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*ptr*) E.t

  val nullable : state -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*ptr*) E.t
  val snull : T.value_type -> state -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*ptr*) E.t
  val snotnull : T.value_type -> state -> T.maybe_nullable -> T.path -> (*ptr*) E.t -> (*ptr*) E.t

  (* Sometimes, we'd like to know in advance how large a serialized value is
   * going to be. Value must have been deserialized into a heap value. *)
  type ssizer = T.maybe_nullable -> T.path -> (*value*) E.t -> ssize
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
  val ssize_of_null : T.maybe_nullable -> T.path -> ssize
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
        pair
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
    pair
      (comment "Desserialize NULL" (Des.dnull t dstate mn0 path src))
      (comment "Serialize NULL" (Ser.snull t sstate mn0 path dst))

  let dsnotnull t sstate dstate mn0 path src dst =
    let open E.Ops in
    pair
      (comment "Desserialize NonNull" (Des.dnotnull t dstate mn0 path src))
      (comment "Serialize NonNull" (Ser.snotnull t sstate mn0 path dst))

  let rec dstup mns transform sstate dstate mn0 path src_dst =
    let open E.Ops in
    let src_dst = comment "Convert a Tuple"
      (E.with_sploded_pair "dstup1" src_dst (fun src dst ->
        pair
          (Des.tup_opn dstate mn0 path mns src)
          (Ser.tup_opn sstate mn0 path mns dst))) in
    let src_dst =
      BatArray.fold_lefti (fun src_dst i _mn ->
        comment ("Convert tuple field "^ Stdlib.string_of_int i)
          (let subpath = T.path_append i path in
          if i = 0 then
            desser_ transform sstate dstate mn0 subpath src_dst
          else
            let src_dst =
              E.with_sploded_pair "dstup2" src_dst (fun src dst ->
                pair
                  (Des.tup_sep i dstate mn0 path src)
                  (Ser.tup_sep i sstate mn0 path dst)) in
            desser_ transform sstate dstate mn0 subpath src_dst)
      ) src_dst mns in
    E.with_sploded_pair "dstup3" src_dst (fun src dst ->
      pair
        (Des.tup_cls dstate mn0 path src)
        (Ser.tup_cls sstate mn0 path dst))

  and dsrec mns transform sstate dstate mn0 path src_dst =
    let open E.Ops in
    let src_dst =
      E.with_sploded_pair "dsrec1" src_dst (fun src dst ->
        pair
          (Des.rec_opn dstate mn0 path mns src)
          (Ser.rec_opn sstate mn0 path mns dst)) in
    let src_dst =
      BatArray.fold_lefti (fun src_dst i (name, _mn) ->
          comment ("Convert record field "^ name)
            (let subpath = T.path_append i path in
            if i = 0 then
              desser_ transform sstate dstate mn0 subpath src_dst
            else
              let src_dst =
                E.with_sploded_pair "dsrec2" src_dst (fun src dst ->
                  pair
                    (Des.rec_sep name dstate mn0 path src)
                    (Ser.rec_sep name sstate mn0 path dst)) in
              desser_ transform sstate dstate mn0 subpath src_dst)
      ) src_dst mns in
    let src_dst = comment "Convert a Record" src_dst in
    E.with_sploded_pair "dsrec3" src_dst (fun src dst ->
      pair
        (Des.rec_cls dstate mn0 path src)
        (Ser.rec_cls sstate mn0 path dst))

  and dssum mns transform sstate dstate mn0 path src_dst =
    let open E.Ops in
    E.with_sploded_pair "dssum1" src_dst (fun src dst ->
      let cstr_src = Des.sum_opn dstate mn0 path mns src in
      let src_dst =
        E.with_sploded_pair "dssum2" cstr_src (fun cstr src ->
          let dst = Ser.sum_opn sstate mn0 path mns cstr dst in
          let src_dst = pair src dst in
          let rec choose_cstr i =
            let max_lbl = Array.length mns - 1 in
            let subpath = T.path_append i path in
            if i >= max_lbl then
              seq [
                assert_ (eq cstr (u16 (Uint16.of_int max_lbl))) ;
                desser_ transform sstate dstate mn0 subpath src_dst ]
            else
              choose
                ~cond:(eq (u16 (Uint16.of_int i)) cstr)
                ~then_:(desser_ transform sstate dstate mn0 subpath src_dst)
                ~else_:(choose_cstr (i + 1)) in
          choose_cstr 0) in
      E.with_sploded_pair "dssum3" src_dst (fun src dst ->
        pair
          (Des.sum_cls dstate mn0 path src)
          (Ser.sum_cls sstate mn0 path dst)))

  (* This will generates a long linear code with one block per array
   * item, which should be ok since vector dimension is expected to be small.
   * TODO: use one of the loop expressions instead if the dimension is large *)
  and dsvec dim mn transform sstate dstate mn0 path src_dst =
    let open E.Ops in
    let src_dst =
      E.with_sploded_pair "dsvec1" src_dst (fun src dst ->
        pair
          (Des.vec_opn dstate mn0 path dim mn src)
          (Ser.vec_opn sstate mn0 path dim mn dst)) in
    let rec loop src_dst i =
      (* Not really required to keep the actual index, as all indices share the
       * same type, but helps with debugging: *)
      let subpath = T.path_append i path in
      if i >= dim then
        E.with_sploded_pair "dsvec2" src_dst (fun src dst ->
          pair
            (Des.vec_cls dstate mn0 path src)
            (Ser.vec_cls sstate mn0 path dst))
      else (
        let src_dst =
          if i = 0 then
            src_dst
          else
            E.with_sploded_pair "dsvec3" src_dst (fun src dst ->
              pair
                (Des.vec_sep i dstate mn0 path src)
                (Ser.vec_sep i sstate mn0 path dst)) in
        (* FIXME: comment is poorly located: *)
        let src_dst =
          comment ("Convert field #"^ Stdlib.string_of_int i)
            (desser_ transform sstate dstate mn0 subpath src_dst) in
        loop src_dst (i + 1)
      )
    in
    let what =
      Printf.sprintf2 "Convert a vector of %d %a"
        dim T.print_maybe_nullable mn in
    comment what (loop src_dst 0)

  and dslist mn transform sstate dstate mn0 path src_dst =
    let open E.Ops in
    let pair_ptrs = T.TPair (Des.ptr mn0, Ser.ptr mn0) in
    (* Pretend we visit only the index 0, which is enough to determine
     * subtypes: *)
    let subpath = T.path_append 0 path in
    (* FIXME: nope. The code emitted in the function below (repeat's body)
     * need to be able to count the nullmask bit index. For this it needs
     * either an accurate path or a distinct call to Ser.nullable (to maintain
     * sstate) per element.
     * So, given we generate a loop, the easier and cleaner is actually to have
     * a dynamic path (ie a path component can be either a compile time int
     * or a run time int).
     * Like we have a runtime fieldmask index in CodeGen_OCaml.
     * Actually, this whole dynamic fieldmask is still missing. We would like
     * dessser to use and maintain a _runtime_ fieldmask and DES should then have
     * a skip method).
     * So for each des/ser callback, it would provide:
     * 1. the typ0 of the underlying fully populated type (compile type)
     * 2. the compile time known path to the current value (as of now) with
     *    unset indices for lists/vectors (ie -1) because they are not needed
     *    to find out the type and field name
     * 3. a runtime unsigned integer giving the actual index in the current
     *    compound container, with which RingBuffer.SER need no state any longer,
     *    therefore we can do away with that state!
     *    Actually, instead of passing it to each type callback it's enough to
     *    pass it to the null/notnull callback but why not all callbacks since
     *    we have it anyway
     * 4. the SER must have a skip in addition to the null callback
     * 5. the DES must also have a skip function called instead of isnull
     * In the short term when fieldmask is just a copy-all, we can keep dessser
     * like it is and merely implements points 3. alone so RingBuffer works
     * and will need no more states.
     * Yet we must not remove states, as heapvalue will need one once fieldmask
     * enter the stage. *)
    comment "Convert a List"
      (E.with_sploded_pair "dslist1" src_dst (fun src dst ->
        (* FIXME: for some deserializers (such as SExpr) it's not easy to
         * know the list length in advance. For those, it would be better
         * to call a distinct 'end-of-list' function returning a bool and
         * read until this returns true. We could simply have both, and
         * here we would repeat_while ~cond:(n<count && not end_of_list),
         * then SEpxr would merely return a very large number of entries
         * (better than to return a single condition in list_opn for non
         * functional backends such as, eventually, C?) *)
        let src_dst =
          match Des.list_opn with
          | KnownSize list_opn ->
              let dim_src = list_opn dstate mn0 path mn src in
              E.with_sploded_pair "dslist2" dim_src (fun dim src ->
                let dst = Ser.list_opn sstate mn0 path mn (Some dim) dst in
                repeat ~from:(i32 0l) ~to_:(to_i32 dim)
                  ~body:(comment "Convert a list item"
                    (E.func2 T.i32 pair_ptrs (fun _l n src_dst ->
                      let src_dst =
                        choose
                          ~cond:(eq n (i32 0l))
                          ~then_:src_dst
                          ~else_:(
                            E.with_sploded_pair "dslist3" src_dst (fun psrc pdst ->
                              pair
                                (Des.list_sep dstate mn0 path psrc)
                                (Ser.list_sep sstate mn0 path pdst))
                          ) in
                      desser_ transform sstate dstate mn0 subpath src_dst)))
                  ~init:(pair src dst))
          | UnknownSize (list_opn, end_of_list) ->
              let t_fst_src_dst = T.TPair (T.bool, pair_ptrs) in
              let src = list_opn dstate mn0 path mn src in
              let dst = Ser.list_opn sstate mn0 path mn None dst in
              let fst_src_dst =
                loop_while
                  ~cond:(comment "Test end of list"
                    (E.func1 t_fst_src_dst (fun _l fst_src_dst ->
                      let src_dst = secnd fst_src_dst in
                      not_ (end_of_list dstate mn0 path (first src_dst)))))
                  ~body:(comment "Convert a list item"
                    (E.func1 t_fst_src_dst (fun _l fst_src_dst ->
                      E.with_sploded_pair "dslist4" fst_src_dst (fun is_first src_dst ->
                        let src_dst =
                          choose
                            ~cond:is_first
                            ~then_:src_dst
                            ~else_:(
                              E.with_sploded_pair "dslist5" src_dst (fun psrc pdst ->
                                pair
                                  (Des.list_sep dstate mn0 path psrc)
                                  (Ser.list_sep sstate mn0 path pdst))) in
                        pair
                          (bool false)
                          (desser_ transform sstate dstate mn0 subpath src_dst)))))
                  ~init:(pair (bool true) (pair src dst)) in
              secnd fst_src_dst
        in
        E.with_sploded_pair "dslist6" src_dst (fun src dst ->
          pair
            (Des.list_cls dstate mn0 path src)
            (Ser.list_cls sstate mn0 path dst))))

  and desser_value_type = function
    | T.Unknown -> invalid_arg "desser_value_type"
    | T.Mac TFloat -> dsfloat
    | T.Mac TString -> dsstring
    | T.Mac TBool -> dsbool
    | T.Mac TChar -> dschar
    | T.Mac TI8 -> dsi8
    | T.Mac TI16 -> dsi16
    | T.Mac TI24 -> dsi24
    | T.Mac TI32 -> dsi32
    | T.Mac TI40 -> dsi40
    | T.Mac TI48 -> dsi48
    | T.Mac TI56 -> dsi56
    | T.Mac TI64 -> dsi64
    | T.Mac TI128 -> dsi128
    | T.Mac TU8 -> dsu8
    | T.Mac TU16 -> dsu16
    | T.Mac TU24 -> dsu24
    | T.Mac TU32 -> dsu32
    | T.Mac TU40 -> dsu40
    | T.Mac TU48 -> dsu48
    | T.Mac TU56 -> dsu56
    | T.Mac TU64 -> dsu64
    | T.Mac TU128 -> dsu128
    | T.Usr vt -> desser_value_type vt.def
    | T.TTup mns -> dstup mns
    | T.TRec mns -> dsrec mns
    | T.TSum mns -> dssum mns
    | T.TVec (dim, mn) -> dsvec dim mn
    | T.TList mn -> dslist mn
    | T.TMap _ -> assert false (* No value of map type *)

  and desser_ transform sstate dstate mn0 path src_dst =
    let open E.Ops in
    let mn = T.type_of_path mn0 path in
    if mn.nullable then (
      E.with_sploded_pair "desser_" src_dst (fun src dst ->
        let cond = Des.is_null dstate mn0 path src in
        (* Des can use [is_null] to prepare for a nullable, but Ser might also
         * have some work to do: *)
        let dst = Ser.nullable sstate mn0 path dst in
        (* XXX WARNING XXX
         * if any of dnull/snull/snotnull/etc update the state, they will
         * do so in both branches of this alternative. *)
        choose ~cond
          ~then_:(dsnull mn.vtyp sstate dstate mn0 path src dst)
          ~else_:(dsnotnull mn.vtyp sstate dstate mn0 path src dst |>
                  desser_value_type mn.vtyp transform sstate dstate mn0 path ))
    ) else (
      desser_value_type mn.vtyp transform sstate dstate mn0 path src_dst
    )

  let desser mn0 ?transform src dst =
    let no_transform _mn0 _path v = v in
    let transform = transform |? no_transform in
    let open E.Ops in
    let sstate, dst = Ser.start mn0 dst
    and dstate, src = Des.start mn0 src in
    let src_dst = pair src dst in
    let src_dst = desser_ transform sstate dstate mn0 [] src_dst in
    E.with_sploded_pair "desser" src_dst (fun src dst ->
      pair
        (Des.stop dstate src)
        (Ser.stop sstate dst))
end

(*
 * Now let's move on to code generators.
 *
 * The idea is that a code generator receives its state, an expression and an
 * optional name for it, and returns an identifier alongside a new state.
 *
 * This state has all defined identifiers.
 *
 * Eventually, the state can be turned into a source file. Unused identifiers
 * may not be included unless they are non-anonymous functions.
 *)

module type BACKEND =
sig
  type state
  val make_state : unit -> state
  val print_definitions : state -> unit IO.output -> unit
  val print_declarations : state -> unit IO.output -> unit
  val print_comment : unit IO.output -> ('a, unit IO.output, unit) format -> 'a

  val add_external_identifier : state -> string -> T.t -> state

  (* Returns the new state, the Identifier expression to use in new expressions,
   * and the identifier name in the source code.
   * Expression is not allowed to have the null type (which would make little
   * sense anyway): *)
  val identifier_of_expression : state -> ?name:string -> E.t -> (state * E.t * string)
  val valid_source_name : string -> string
  val preferred_def_extension : string
  val preferred_decl_extension : string
  val compile_cmd : optim:int -> link:bool -> string -> string -> string
end
