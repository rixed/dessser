open Batteries
open Stdint
open DessserTypes
open DessserExpressions

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
  val ptr : maybe_nullable -> typ (* either dataptr or valueptr *)

  val start : maybe_nullable -> (*ptr*) e -> state * (*ptr*) e
  val stop : state -> (*ptr*) e -> (*ptr*) e

  (* A basic value deserializer takes a state, an expression
   * yielding a pointer (either a CodePtr pointing at a byte stream or a
   * ValuePtr pointing at a heap value of type ['b]), and returns two
   * expressions: one yielding the advanced pointer (of the exact same type) and
   * one yielding the value that's been deserialized from the given location: *)
  (* FIXME: make this type "private": *)
  type des = state -> (*ptr*) e -> (* (nn * ptr) *) e

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

  val tup_opn : state -> maybe_nullable array -> (*ptr*) e -> (*ptr*) e
  val tup_cls : state -> (*ptr*) e -> (*ptr*) e
  val tup_sep : int (* before *) -> state -> (*ptr*) e -> (*ptr*) e
  val rec_opn : state -> (string * maybe_nullable) array -> (*ptr*) e -> (*ptr*) e
  val rec_cls : state -> (*ptr*) e -> (*ptr*) e
  val rec_sep : string (* before *) -> state -> (*ptr*) e -> (*ptr*) e
  val vec_opn : state -> (*dim*) int -> maybe_nullable -> (*ptr*) e -> (*ptr*) e
  val vec_cls : state -> (*ptr*) e -> (*ptr*) e
  val vec_sep : int (* before *) -> state -> (*ptr*) e -> (*ptr*) e
  val list_opn : state -> maybe_nullable -> (*ptr*) e -> (* (nn * ptr) *) e
  val list_cls : state -> (*ptr*) e -> (*ptr*) e
  val list_sep : state -> (*ptr*) e -> (*ptr*) e

  val is_null : state -> (*ptr*) e -> (*bool*) e
  val dnull : value_type -> state -> (*ptr*) e -> (*ptr*) e
  val dnotnull : value_type -> state -> (*ptr*) e -> (*ptr*) e
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
  type ser = state -> (*nn*) e -> (*ptr*) e -> (*ptr*) e

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

  val tup_opn : state -> maybe_nullable array -> (*ptr*) e -> (*ptr*) e
  val tup_cls : state -> (*ptr*) e -> (*ptr*) e
  val tup_sep : int (* before *) -> state -> (*ptr*) e -> (*ptr*) e
  val rec_opn : state -> (string * maybe_nullable) array -> (*ptr*) e -> (*ptr*) e
  val rec_cls : state -> (*ptr*) e -> (*ptr*) e
  val rec_sep : string (* before *) -> state -> (*ptr*) e -> (*ptr*) e
  val vec_opn : state -> (*dim*) int -> maybe_nullable -> (*ptr*) e -> (*ptr*) e
  val vec_cls : state -> (*ptr*) e -> (*ptr*) e
  val vec_sep : int (* before *) -> state -> (*ptr*) e -> (*ptr*) e
  val list_opn : state -> maybe_nullable -> (*ptr*) e -> (*nn*) e -> (*ptr*) e
  val list_cls : state -> (*ptr*) e -> (*ptr*) e
  val list_sep : state -> (*ptr*) e -> (*ptr*) e

  val nullable : state -> (*ptr*) e -> (*ptr*) e
  val snull : value_type -> state -> (*ptr*) e -> (*ptr*) e
  val snotnull : value_type -> state -> (*ptr*) e -> (*ptr*) e

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
   * [vtyp] denotes the maybe_nullable of the current subfields, whereas
   * [mtyp0] denotes the maybe_nullable of the whole value. *)
  let ds ser des typ sstate dstate mtyp0 src_dst =
    let what = IO.to_string print_typ typ in
    let src_dst = Comment ("Desserialize a "^ what, src_dst) in
    MapPair (src_dst,
      func [|Des.ptr mtyp0; Ser.ptr mtyp0|] (fun fid ->
        let v_src = des dstate (Param (fid, 0)) in
        let dst = Param (fid, 1) in
        with_sploded_pair "ds" v_src (fun v src ->
          (* dessser handle nulls itself, so that DES/SER implementations
           * do not have to care for nullability. NotNullable is just a cast
           * and has no other effect outside of type_check. *)
          Pair (
            src,
            Comment ("Serialize a "^ what, ser sstate v dst)))))

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

  let dsnull t sstate dstate mtyp0 src_dst =
    MapPair (src_dst,
      func [|Des.ptr mtyp0; Ser.ptr mtyp0|] (fun fid -> Pair (
        Comment ("Desserialize NULL", Des.dnull t dstate (Param (fid, 0))),
        Comment ("Serialize NULL", Ser.snull t sstate (Param (fid, 1))))))

  let dsnotnull t sstate dstate mtyp0 src_dst =
    MapPair (src_dst,
      func [|Des.ptr mtyp0; Ser.ptr mtyp0|] (fun fid -> Pair (
        Comment ("Desserialize NonNull", Des.dnotnull t dstate (Param (fid, 0))),
        Comment ("Serialize NonNull", Ser.snotnull t sstate (Param (fid, 1))))))

  let rec dstup mtyps sstate dstate mtyp0 src_dst =
    let src_dst = Comment ("Convert a Tuple",
      MapPair (src_dst,
        func [|Des.ptr mtyp0; Ser.ptr mtyp0|] (fun fid -> Pair (
          Des.tup_opn dstate mtyps (Param (fid, 0)),
          Ser.tup_opn sstate mtyps (Param (fid, 1)))))) in
    let src_dst =
      BatArray.fold_lefti (fun src_dst i vtyp ->
        let src_dst = Comment ("Convert tuple field "^ string_of_int i, src_dst) in
        if i = 0 then
          desser_ vtyp sstate dstate mtyp0 src_dst
        else
          let src_dst = MapPair (src_dst,
            func [|Des.ptr mtyp0; Ser.ptr mtyp0|] (fun fid ->
              Pair (
                Des.tup_sep i dstate (Param (fid, 0)),
                Ser.tup_sep i sstate (Param (fid, 1))))) in
          desser_ vtyp sstate dstate mtyp0 src_dst
      ) src_dst mtyps in
    MapPair (src_dst,
      func [|Des.ptr mtyp0; Ser.ptr mtyp0|] (fun fid -> Pair (
        Des.tup_cls dstate (Param (fid, 0)),
        Ser.tup_cls sstate (Param (fid, 1)))))

  and dsrec mtyps sstate dstate mtyp0 src_dst =
    let src_dst = Comment ("Convert a Record",
      MapPair (src_dst,
        func [|Des.ptr mtyp0; Ser.ptr mtyp0|] (fun fid -> Pair (
          Des.rec_opn dstate mtyps (Param (fid, 0)),
          Ser.rec_opn sstate mtyps (Param (fid, 1)))))) in
    let src_dst =
      BatArray.fold_lefti (fun src_dst i (name, vtyp) ->
        let src_dst = Comment ("Convert record field "^ name, src_dst) in
        if i = 0 then
          desser_ vtyp sstate dstate mtyp0 src_dst
        else
          let src_dst = MapPair (src_dst,
            func [|Des.ptr mtyp0; Ser.ptr mtyp0|] (fun fid ->
              Pair (
                Des.rec_sep name dstate (Param (fid, 0)),
                Ser.rec_sep name sstate (Param (fid, 1))))) in
          desser_ vtyp sstate dstate mtyp0 src_dst
      ) src_dst mtyps in
    MapPair (src_dst,
      func [|Des.ptr mtyp0; Ser.ptr mtyp0|] (fun fid -> Pair (
        Des.rec_cls dstate (Param (fid, 0)),
        Ser.rec_cls sstate (Param (fid, 1)))))

  (* This will generates a long linear code with one block per array
   * item, which should be ok since vector dimension is expected to be small.
   * TODO: use one of the loop expressions instead if the dimension is large *)
  and dsvec dim vtyp sstate dstate mtyp0 src_dst =
    let src_dst = Comment ("Convert a Vector",
      MapPair (src_dst,
        func [|Des.ptr mtyp0; Ser.ptr mtyp0|] (fun fid -> Pair (
          Des.vec_opn dstate dim vtyp (Param (fid, 0)),
          Ser.vec_opn sstate dim vtyp (Param (fid, 1)))))) in
    let rec loop src_dst i =
      if i >= dim then
        MapPair (src_dst,
          func [|Des.ptr mtyp0; Ser.ptr mtyp0|] (fun fid -> Pair (
            Des.vec_cls dstate (Param (fid, 0)),
            Ser.vec_cls sstate (Param (fid, 1)))))
      else (
        let src_dst = Comment ("Convert vector field "^ string_of_int i, src_dst) in
        if i = 0 then (
          let src_dst = desser_ vtyp sstate dstate mtyp0 src_dst in
          loop src_dst (i + 1)
        ) else (
          let src_dst = MapPair (src_dst,
            func [|Des.ptr mtyp0; Ser.ptr mtyp0|] (fun fid -> Pair (
              Des.vec_sep i dstate (Param (fid, 0)),
              Ser.vec_sep i sstate (Param (fid, 1))))) in
          let src_dst = desser_ vtyp sstate dstate mtyp0 src_dst in
          loop src_dst (i + 1)
        )
      )
    in
    loop src_dst 0

  and dslist vtyp sstate dstate mtyp0 src_dst =
    let pair_ptrs = TPair (Des.ptr mtyp0, Ser.ptr mtyp0) in
    Comment ("Convert a List",
      with_sploded_pair "dslist1" src_dst (fun src dst ->
        let dim_src = Des.list_opn dstate vtyp src in
        with_sploded_pair "dslist2" dim_src (fun dim src ->
          let dst = Ser.list_opn sstate vtyp dst dim in
          let src_dst =
            Repeat (I32 0l, ToI32 dim,
              Comment ("Convert a list item",
                func [|i32; pair_ptrs|] (fun fid -> (
                  let param_n = Param (fid, 0) (*i32*) in
                  let param_src_dst = Param (fid, 1) (*pair_ptrs*) in
                  let src_dst =
                    Choose (Eq (param_n, I32 0l),
                      param_src_dst,
                      MapPair (param_src_dst,
                        func [|Des.ptr mtyp0; Ser.ptr mtyp0|] (fun fid -> Pair (
                          Des.list_sep dstate (Param (fid, 0)),
                          Ser.list_sep sstate (Param (fid, 1)))))) in
                  desser_ vtyp sstate dstate mtyp0 src_dst))),
              Pair (src, dst)) in
          MapPair (src_dst,
            func [|Des.ptr mtyp0; Ser.ptr mtyp0|] (fun fid -> Pair (
              Des.vec_cls dstate (Param (fid, 0)),
              Ser.vec_cls sstate (Param (fid, 1))))))))

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
    | Usr t -> desser_value_type t.def
    | TTup mtyps -> dstup mtyps
    | TRec mtyps -> dsrec mtyps
    | TVec (dim, vtyp) -> dsvec dim vtyp
    | TList vtyp -> dslist vtyp
    | TMap _ -> assert false (* No value of map type *)

  and desser_ vtyp sstate dstate mtyp0 src_dst =
    match vtyp with
    | Nullable t ->
        with_sploded_pair "desser_" src_dst (fun src dst ->
          let cond = Des.is_null dstate src in
          (* Des can use [is_null] to prepare for a nullable, but Ser might also
           * have some work to do: *)
          let dst = Ser.nullable sstate dst in
          let src_dst = Pair (src, dst) in
          (* XXX WARNING XXX
           * if any of dnull/snull/snotnull/etc update the state, they will
           * do so in both branches of this alternative. *)
          Choose (cond,
            dsnull t sstate dstate mtyp0 src_dst,
            (
              let src_dst = dsnotnull t sstate dstate mtyp0 src_dst in
              desser_value_type t sstate dstate mtyp0 src_dst
            )))
    | NotNullable t ->
        desser_value_type t sstate dstate mtyp0 src_dst

  let desser mtyp0 src dst =
    let sstate, dst = Ser.start mtyp0 dst
    and dstate, src = Des.start mtyp0 src in
    let src_dst = Pair (src, dst) in
    let src_dst = desser_ mtyp0 sstate dstate mtyp0 src_dst in
    MapPair (src_dst,
      func [|Des.ptr mtyp0; Ser.ptr mtyp0|] (fun fid -> Pair (
        Des.stop dstate (Param (fid, 0)),
        Ser.stop sstate (Param (fid, 1)))))
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
  val print_source : state -> 'a IO.output -> unit
  (* Returns the new state, the Identifier expression to use in new expressions,
   * and the identifier name in the source code: *)
  val identifier_of_expression : state -> ?name:string -> e -> (state * e * string)
  val preferred_file_extension : string
end
