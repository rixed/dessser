open Batteries
open Stdint
open DessserTypes
open DessserExpressions
open DessserTools
module T = DessserTypes

(* Used by deserializers to "open" lists: *)
type 'a list_opener =
  (* When a list size is known from the beginning, implement this that
   * returns both the list size and the new src pointer: *)
  | KnownSize of ('a -> maybe_nullable -> path -> maybe_nullable -> (*ptr*) e -> (* (u32 * ptr) *) e)
  (* Whereas when the list size is not known beforehand, rather implement
   * this pair of functions, one to parse the ilst header and return the new
   * src pointer and one that will be called before any new token and must
   * return true if the list is finished: *)
  | UnknownSize of
      ('a -> maybe_nullable -> path -> maybe_nullable -> (*ptr*) e -> (*ptr*) e) *
      ('a -> maybe_nullable -> path -> (*ptr*) e -> (*bool*) e)

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
  val ptr : maybe_nullable -> typ (* either dataptr or valueptr, or whatever really  *)

  val start : maybe_nullable -> (*ptr*) e -> state * (*ptr?*) e
  val stop : state -> (*ptr?*) e -> (*ptr*) e

  (* A basic value deserializer takes a state, an expression
   * yielding a pointer (either a CodePtr pointing at a byte stream or a
   * ValuePtr pointing at a heap value of type ['b]), and returns two
   * expressions: one yielding the advanced pointer (of the exact same type) and
   * one yielding the value that's been deserialized from the given location.
   * The [maybe_nullable] and [path] values are in the fully fledged global
   * type and therefore must be used cautiously! *)
  (* FIXME: make this type "private": *)
  type des = state -> maybe_nullable -> path -> (*ptr*) e -> (*v*ptr*) e

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
  val tup_opn : state -> maybe_nullable -> path -> maybe_nullable array -> (*ptr*) e -> (*ptr*) e
  val tup_cls : state -> maybe_nullable -> path -> (*ptr*) e -> (*ptr*) e
  val tup_sep : int (* before *) -> state -> maybe_nullable -> path -> (*ptr*) e -> (*ptr*) e
  val rec_opn : state -> maybe_nullable -> path -> (string * maybe_nullable) array -> (*ptr*) e -> (*ptr*) e
  val rec_cls : state -> maybe_nullable -> path -> (*ptr*) e -> (*ptr*) e
  val rec_sep : string (* before *) -> state -> maybe_nullable -> path -> (*ptr*) e -> (*ptr*) e
  val vec_opn : state -> maybe_nullable -> path -> (*dim*) int -> maybe_nullable -> (*ptr*) e -> (*ptr*) e
  val vec_cls : state -> maybe_nullable -> path -> (*ptr*) e -> (*ptr*) e
  val vec_sep : int (* before *) -> state -> maybe_nullable -> path -> (*ptr*) e -> (*ptr*) e
  val list_opn : state list_opener
  val list_cls : state -> maybe_nullable -> path -> (*ptr*) e -> (*ptr*) e
  val list_sep : state -> maybe_nullable -> path -> (*ptr*) e -> (*ptr*) e

  val is_null : state -> maybe_nullable -> path -> (*ptr*) e -> (*bool*) e
  val dnull : value_type -> state -> maybe_nullable -> path -> (*ptr*) e -> (*ptr*) e
  val dnotnull : value_type -> state -> maybe_nullable -> path -> (*ptr*) e -> (*ptr*) e
end

(* Same goes for SER(rializers), with the addition that it is also possible to
 * "serialize" into a heap value instead of a data stream:
 * (note: "ssize" stands for "serialized size") *)
type ssize = ConstSize of int | DynSize of (*size*) e

module type SER =
sig
  (* RW state passed to every serialization operations *)
  type state
  val ptr : maybe_nullable -> typ (* either dataptr or valueptr *)

  val start : maybe_nullable -> (*ptr*) e -> state * (*ptr*) e
  val stop : state -> (*ptr*) e -> (*ptr*) e

  (* FIXME: make this type "private": *)
  type ser = state -> maybe_nullable -> path -> (*v*) e -> (*ptr*) e -> (*ptr*) e

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
  val tup_opn : state -> maybe_nullable -> path -> maybe_nullable array -> (*ptr*) e -> (*ptr*) e
  val tup_cls : state -> maybe_nullable -> path -> (*ptr*) e -> (*ptr*) e
  val tup_sep : int (* before *) -> state -> maybe_nullable -> path -> (*ptr*) e -> (*ptr*) e
  val rec_opn : state -> maybe_nullable -> path -> (string * maybe_nullable) array -> (*ptr*) e -> (*ptr*) e
  val rec_cls : state -> maybe_nullable -> path -> (*ptr*) e -> (*ptr*) e
  val rec_sep : string (* before *) -> state -> maybe_nullable -> path -> (*ptr*) e -> (*ptr*) e
  val vec_opn : state -> maybe_nullable -> path -> (*dim*) int -> maybe_nullable -> (*ptr*) e -> (*ptr*) e
  val vec_cls : state -> maybe_nullable -> path -> (*ptr*) e -> (*ptr*) e
  val vec_sep : int (* before *) -> state -> maybe_nullable -> path -> (*ptr*) e -> (*ptr*) e
  val list_opn : state -> maybe_nullable -> path -> maybe_nullable -> (*u32*) e option -> (*ptr*) e -> (*ptr*) e
  val list_cls : state -> maybe_nullable -> path -> (*ptr*) e -> (*ptr*) e
  val list_sep : state -> maybe_nullable -> path -> (*ptr*) e -> (*ptr*) e

  val nullable : state -> maybe_nullable -> path -> (*ptr*) e -> (*ptr*) e
  val snull : value_type -> state -> maybe_nullable -> path -> (*ptr*) e -> (*ptr*) e
  val snotnull : value_type -> state -> maybe_nullable -> path -> (*ptr*) e -> (*ptr*) e

  (* Sometimes, we'd like to know in advance how large a serialized value is
   * going to be. Value must have been deserialized into a heap value. *)
  type ssizer = maybe_nullable -> path -> (*value*) e -> ssize
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
  val ssize_of_vec : ssizer
  val ssize_of_list : ssizer
  val ssize_of_null : maybe_nullable -> path -> ssize
end

(* Now we can combine a DES and a SER to create a converter from one format
 * into another (including heap values, with some trickery) *)
module DesSer (Des : DES) (Ser : SER) =
struct
  (* Most of the functions below return the src and dst pointers advanced to
   * point to the next value to read/write.
   * [mn] denotes the maybe_nullable of the current subfields, whereas
   * [mn0] denotes the maybe_nullable of the whole value. *)
  let ds ser des typ transform sstate dstate mn0 path src_dst =
    let what = IO.to_string print_typ typ in
    with_sploded_pair "ds1" src_dst (fun src dst ->
      let v_src = des dstate mn0 path src in
      with_sploded_pair "ds2" v_src (fun v src ->
        let v = transform mn0 path v in
        let open Ops in
        (* dessser handle nulls itself, so that DES/SER implementations
         * do not have to care for nullability. NotNullable is just a cast
         * and has no other effect outside of type_check. *)
        pair
          (comment ("Desserialize a "^ what) src)
          (comment ("Serialize a "^ what) (ser sstate mn0 path v dst))))

  let dsfloat = ds Ser.sfloat Des.dfloat float
  let dsstring = ds Ser.sstring Des.dstring string
  let dsbool = ds Ser.sbool Des.dbool bool
  let dschar = ds Ser.schar Des.dchar char
  let dsi8 = ds Ser.si8 Des.di8 i8
  let dsi16 = ds Ser.si16 Des.di16 i16
  let dsi24 = ds Ser.si24 Des.di24 i24
  let dsi32 = ds Ser.si32 Des.di32 i32
  let dsi40 = ds Ser.si40 Des.di40 i40
  let dsi48 = ds Ser.si48 Des.di48 i48
  let dsi56 = ds Ser.si56 Des.di56 i56
  let dsi64 = ds Ser.si64 Des.di64 i64
  let dsi128 = ds Ser.si128 Des.di128 i128
  let dsu8 = ds Ser.su8 Des.du8 u8
  let dsu16 = ds Ser.su16 Des.du16 u16
  let dsu24 = ds Ser.su24 Des.du24 u24
  let dsu32 = ds Ser.su32 Des.du32 u32
  let dsu40 = ds Ser.su40 Des.du40 u40
  let dsu48 = ds Ser.su48 Des.du48 u48
  let dsu56 = ds Ser.su56 Des.du56 u56
  let dsu64 = ds Ser.su64 Des.du64 u64
  let dsu128 = ds Ser.su128 Des.du128 u128

  let dsnull t sstate dstate mn0 path src dst =
    let open Ops in
    pair
      (comment "Desserialize NULL" (Des.dnull t dstate mn0 path src))
      (comment "Serialize NULL" (Ser.snull t sstate mn0 path dst))

  let dsnotnull t sstate dstate mn0 path src dst =
    let open Ops in
    pair
      (comment "Desserialize NonNull" (Des.dnotnull t dstate mn0 path src))
      (comment "Serialize NonNull" (Ser.snotnull t sstate mn0 path dst))

  let rec dstup mns transform sstate dstate mn0 path src_dst =
    let open Ops in
    let src_dst = comment "Convert a Tuple"
      (with_sploded_pair "dstup1" src_dst (fun src dst ->
        pair
          (Des.tup_opn dstate mn0 path mns src)
          (Ser.tup_opn sstate mn0 path mns dst))) in
    let src_dst =
      BatArray.fold_lefti (fun src_dst i _mn ->
        comment ("Convert tuple field "^ Pervasives.string_of_int i)
          (let subpath = path_append i path in
          if i = 0 then
            desser_ transform sstate dstate mn0 subpath src_dst
          else
            let src_dst =
              with_sploded_pair "dstup2" src_dst (fun src dst ->
                pair
                  (Des.tup_sep i dstate mn0 path src)
                  (Ser.tup_sep i sstate mn0 path dst)) in
            desser_ transform sstate dstate mn0 subpath src_dst)
      ) src_dst mns in
    with_sploded_pair "dstup3" src_dst (fun src dst ->
      pair
        (Des.tup_cls dstate mn0 path src)
        (Ser.tup_cls sstate mn0 path dst))

  and dsrec mns transform sstate dstate mn0 path src_dst =
    let open Ops in
    let src_dst =
      with_sploded_pair "dsrec1" src_dst (fun src dst ->
        pair
          (Des.rec_opn dstate mn0 path mns src)
          (Ser.rec_opn sstate mn0 path mns dst)) in
    let src_dst =
      BatArray.fold_lefti (fun src_dst i (name, _mn) ->
          comment ("Convert record field "^ name)
            (let subpath = path_append i path in
            if i = 0 then
              desser_ transform sstate dstate mn0 subpath src_dst
            else
              let src_dst =
                with_sploded_pair "dsrec2" src_dst (fun src dst ->
                  pair
                    (Des.rec_sep name dstate mn0 path src)
                    (Ser.rec_sep name sstate mn0 path dst)) in
              desser_ transform sstate dstate mn0 subpath src_dst)
      ) src_dst mns in
    let src_dst = comment "Convert a Record" src_dst in
    with_sploded_pair "dsrec3" src_dst (fun src dst ->
      pair
        (Des.rec_cls dstate mn0 path src)
        (Ser.rec_cls sstate mn0 path dst))

  (* This will generates a long linear code with one block per array
   * item, which should be ok since vector dimension is expected to be small.
   * TODO: use one of the loop expressions instead if the dimension is large *)
  and dsvec dim mn transform sstate dstate mn0 path src_dst =
    let open Ops in
    let src_dst =
      with_sploded_pair "dsvec1" src_dst (fun src dst ->
        pair
          (Des.vec_opn dstate mn0 path dim mn src)
          (Ser.vec_opn sstate mn0 path dim mn dst)) in
    let rec loop src_dst i =
      (* Not really required to keep the actual index, as all indices share the
       * same type, but helps with debugging: *)
      let subpath = path_append i path in
      if i >= dim then
        with_sploded_pair "dsvec2" src_dst (fun src dst ->
          pair
            (Des.vec_cls dstate mn0 path src)
            (Ser.vec_cls sstate mn0 path dst))
      else (
        let src_dst =
          if i = 0 then
            src_dst
          else
            with_sploded_pair "dsvec3" src_dst (fun src dst ->
              pair
                (Des.vec_sep i dstate mn0 path src)
                (Ser.vec_sep i sstate mn0 path dst)) in
        (* FIXME: comment is poorly located: *)
        let src_dst =
          comment ("Convert field #"^ Pervasives.string_of_int i)
            (desser_ transform sstate dstate mn0 subpath src_dst) in
        loop src_dst (i + 1)
      )
    in
    let what =
      Printf.sprintf2 "Convert a vector of %d %a"
        dim print_maybe_nullable mn in
    comment what (loop src_dst 0)

  and dslist mn transform sstate dstate mn0 path src_dst =
    let open Ops in
    let pair_ptrs = TPair (Des.ptr mn0, Ser.ptr mn0) in
    (* Pretend we visit only the index 0, which is enough to determine
     * subtypes: *)
    let subpath = path_append 0 path in
    (* FIXME: nope. The code emitted in the function bolow (repeat's body)
     * need to be able to count the nullmask bit index. For this is need
     * either an accurate path or a distinct call to Ser.nullable (to maintain
     * sstate) per element.
     * So, given we generate a loop, the easier and cleaner is actualy to have
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
      (with_sploded_pair "dslist1" src_dst (fun src dst ->
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
              with_sploded_pair "dslist2" dim_src (fun dim src ->
                let dst = Ser.list_opn sstate mn0 path mn (Some dim) dst in
                repeat ~from:(i32 0l) ~to_:(to_i32 dim)
                  ~body:(comment "Convert a list item"
                    (func2 T.i32 pair_ptrs (fun n src_dst ->
                      let src_dst =
                        choose
                          ~cond:(eq n (i32 0l))
                          src_dst
                          (with_sploded_pair "dslist3" src_dst (fun psrc pdst ->
                            pair
                              (Des.list_sep dstate mn0 path psrc)
                              (Ser.list_sep sstate mn0 path pdst))) in
                      desser_ transform sstate dstate mn0 subpath src_dst)))
                  ~init:(pair src dst))
          | UnknownSize (list_opn, end_of_list) ->
              let t_fst_src_dst = TPair (T.bool, pair_ptrs) in
              let src = list_opn dstate mn0 path mn src in
              let dst = Ser.list_opn sstate mn0 path mn None dst in
              let fst_src_dst =
                loop_while
                  ~cond:(comment "Test end of list"
                    (func1 t_fst_src_dst (fun fst_src_dst ->
                      let src_dst = secnd fst_src_dst in
                      not_ (end_of_list dstate mn0 path (first src_dst)))))
                  ~body:(comment "Convert a list item"
                    (func1 t_fst_src_dst (fun fst_src_dst ->
                      with_sploded_pair "dslist4" fst_src_dst (fun is_first src_dst ->
                        let src_dst =
                          choose
                            ~cond:is_first
                            src_dst
                            (with_sploded_pair "dslist5" src_dst (fun psrc pdst ->
                              pair
                                (Des.list_sep dstate mn0 path psrc)
                                (Ser.list_sep sstate mn0 path pdst))) in
                        pair
                          (bool false)
                          (desser_ transform sstate dstate mn0 subpath src_dst)))))
                  ~init:(pair (bool true) (pair src dst)) in
              secnd fst_src_dst
        in
        with_sploded_pair "dslist6" src_dst (fun src dst ->
          pair
            (Des.list_cls dstate mn0 path src)
            (Ser.list_cls sstate mn0 path dst))))

  and desser_value_type = function
    | Mac TFloat -> dsfloat
    | Mac TString -> dsstring
    | Mac TBool -> dsbool
    | Mac TChar -> dschar
    | Mac TI8 -> dsi8
    | Mac TI16 -> dsi16
    | Mac TI24 -> dsi24
    | Mac TI32 -> dsi32
    | Mac TI40 -> dsi40
    | Mac TI48 -> dsi48
    | Mac TI56 -> dsi56
    | Mac TI64 -> dsi64
    | Mac TI128 -> dsi128
    | Mac TU8 -> dsu8
    | Mac TU16 -> dsu16
    | Mac TU24 -> dsu24
    | Mac TU32 -> dsu32
    | Mac TU40 -> dsu40
    | Mac TU48 -> dsu48
    | Mac TU56 -> dsu56
    | Mac TU64 -> dsu64
    | Mac TU128 -> dsu128
    | Usr vt -> desser_value_type vt.def
    | TTup mns -> dstup mns
    | TRec mns -> dsrec mns
    | TVec (dim, mn) -> dsvec dim mn
    | TList mn -> dslist mn
    | TMap _ -> assert false (* No value of map type *)

  and desser_ transform sstate dstate mn0 path src_dst =
    let open Ops in
    let mn = type_of_path mn0 path in
    match mn with
    | Nullable vt ->
        with_sploded_pair "desser_" src_dst (fun src dst ->
          let cond = Des.is_null dstate mn0 path src in
          (* Des can use [is_null] to prepare for a nullable, but Ser might also
           * have some work to do: *)
          let dst = Ser.nullable sstate mn0 path dst in
          (* XXX WARNING XXX
           * if any of dnull/snull/snotnull/etc update the state, they will
           * do so in both branches of this alternative. *)
          choose ~cond
            (dsnull vt sstate dstate mn0 path src dst)
            (dsnotnull vt sstate dstate mn0 path src dst |>
             desser_value_type vt transform sstate dstate mn0 path ))
    | NotNullable vt ->
        desser_value_type vt transform sstate dstate mn0 path src_dst

  let desser mn0 ?transform src dst =
    let no_transform _mn0 _path v = v in
    let transform = transform |? no_transform in
    let open Ops in
    let sstate, dst = Ser.start mn0 dst
    and dstate, src = Des.start mn0 src in
    let src_dst = pair src dst in
    let src_dst = desser_ transform sstate dstate mn0 [] src_dst in
    with_sploded_pair "desser" src_dst (fun src dst ->
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

  val add_external_identifier : state -> string -> typ -> state

  (* Returns the new state, the Identifier expression to use in new expressions,
   * and the identifier name in the source code.
   * Expression is not allowed to have the null type (which would make little
   * sense anyway): *)
  val identifier_of_expression : state -> ?name:string -> e -> (state * e * string)
  val preferred_def_extension : string
  val preferred_decl_extension : string
  val compile_cmd : optim:int -> link:bool -> string -> string -> string
end
