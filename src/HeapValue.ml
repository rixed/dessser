(* Dessser API is optimised to convert from one format into another without
 * materializing complex values in memory.
 * But when we do want to materialize the value in the heap, another API is
 * more natural.
 * The two following modules are designed to unserialize into a materialized
 * values and to then serialize that materialized value. *)
open Batteries
open Stdint
open Dessser
open DessserTools
module T = DessserTypes
module E = DessserExpressions
open E.Ops

module Materialize (Des : DES) : sig
    val make : ?config:Des.config -> T.maybe_nullable -> (*src*) E.t -> (*e*src*) E.t
  end =
struct

  let rec dvec dim mn dstate mn0 path src =
    let src = Des.vec_opn dstate mn0 path dim mn src in
    let rec loop ids i src =
      if i >= dim then
        pair
          (make_vec (List.rev ids))
          (Des.vec_cls dstate mn0 path src)
      else
        let src = if i = 0 then src else
                    Des.vec_sep dstate mn0 path src in
        let subpath = T.path_append i path in
        let v_src = make1 dstate mn0 subpath mn src in
        E.with_sploded_pair "dvec" v_src (fun v src ->
          loop (v :: ids) (i + 1) src) in
    loop [] 0 src

  (* Lists and sets are serialized the same, so here the list is desserialized
   * into an slist, and then the final operation converts it into a list or a
   * set: *)

  and dlist mn dstate mn0 path src =
    dslist list_of_slist_rev mn dstate mn0 path src

  and dset mn dstate mn0 path src =
    dslist set_of_slist mn dstate mn0 path src

  and dslist of_slist mn dstate mn0 path src =
    let init_t = T.TValue mn in
    let init_list_t = T.TSList init_t in
    let inits_src_t = T.TPair (init_list_t, Des.ptr mn0) in
    (* good enough to determine the item type but not much more: *)
    let subpath = T.path_append 0 path in
    match Des.list_opn dstate with
    | KnownSize list_opn ->
        let dim_src = list_opn mn0 path mn src in
        let inits_src =
          E.with_sploded_pair "dlist1" dim_src (fun dim src ->
            repeat ~from:(i32 0l) ~to_:(to_i32 dim)
              ~body:
                (E.func2 T.i32 inits_src_t (fun _l n inits_src ->
                  E.with_sploded_pair "dlist2" inits_src (fun inits src ->
                    let src =
                      if_ ~cond:(eq n (i32 0l))
                        ~then_:src
                        ~else_:(Des.list_sep dstate mn0 path src) in
                    let v_src = make1 dstate mn0 subpath mn src in
                    E.with_sploded_pair "dlist3" v_src (fun v src ->
                      pair (cons v inits) src))))
              ~init:(pair (end_of_list init_t) src)) in
        E.with_sploded_pair "dlist4" inits_src (fun inits src ->
          let v = of_slist inits
          and src = Des.list_cls dstate mn0 path src in
          pair v src)
    | UnknownSize (list_opn, is_end_of_list) ->
        let fst_inits_src_t = T.TPair (T.bool, inits_src_t) in
        let src = list_opn mn0 path mn src in
        let fst_inits_src =
          loop_while
            ~cond:
              (E.func1 fst_inits_src_t (fun _l fst_inits_src ->
                let src = secnd (secnd fst_inits_src) in
                not_ (is_end_of_list mn0 path src)))
            ~body:
              (E.func1 fst_inits_src_t (fun _l fst_inits_src ->
                E.with_sploded_pair "dlist5" fst_inits_src (fun is_fst inits_src ->
                  E.with_sploded_pair "dlist6" inits_src (fun inits src ->
                    let src =
                      if_ ~cond:is_fst
                        ~then_:src
                        ~else_:(Des.list_sep dstate mn0 path src) in
                    let v_src = make1 dstate mn0 subpath mn src in
                    let inits_src =
                      E.with_sploded_pair "dlist7" v_src (fun v src ->
                        pair (cons v inits) src) in
                    pair (bool false) inits_src))))
            ~init:(pair (bool true) (pair (end_of_list init_t) src)) in
        E.with_sploded_pair "dlist9" (secnd fst_inits_src) (fun inits src ->
          let v = of_slist inits
          and src = Des.list_cls dstate mn0 path src in
          pair v src)

  and dtup mns dstate mn0 path src =
    let src = Des.tup_opn dstate mn0 path mns src in
    let rec loop ids i src =
      if i >= Array.length mns then
        pair
          (make_tup (List.rev ids))
          (Des.tup_cls dstate mn0 path src)
      else
        let src = if i = 0 then src else
                    Des.tup_sep dstate mn0 path src in
        let subpath = T.path_append i path in
        let v_src = make1 dstate mn0 subpath mns.(i) src in
        E.with_sploded_pair "dtup" v_src (fun v src ->
          loop (v :: ids) (i + 1) src) in
    loop [] 0 src

  and drec mns dstate mn0 path src =
    let src = Des.rec_opn dstate mn0 path mns src in
    let len = Array.length mns in
    let rec loop ids i src =
      if i >= len then
        pair
          (make_rec (List.fold_lefti (fun inits i id ->
             string (fst mns.(len - i - 1)) :: id :: inits) [] ids))
          (Des.rec_cls dstate mn0 path src)
      else
        let src = if i = 0 then src else
                    Des.rec_sep dstate mn0 path src in
        let subpath = T.path_append i path in
        let v_src = make1 dstate mn0 subpath (snd mns.(i)) src in
        E.with_sploded_pair "drec" v_src (fun v src ->
          loop (v :: ids) (i + 1) src) in
    loop [] 0 src

  and dsum mns dstate mn0 path src =
    let cstr_src = Des.sum_opn dstate mn0 path mns src in
    let max_lbl = Array.length mns - 1 in
    E.with_sploded_pair "dsum1" cstr_src (fun cstr src ->
      let rec choose_cstr i =
        assert (i <= max_lbl) ;
        let res () =
          let subpath = T.path_append i path in
          let _, subtyp = mns.(i) in
          let v_src = make1 dstate mn0 subpath subtyp src in
          E.with_sploded_pair "dsum2" v_src (fun v src ->
            pair
              (construct mns i v)
              (Des.sum_cls dstate mn0 path src))
        in
        if i = max_lbl then
          seq [
            assert_ (eq cstr (u16 (Uint16.of_int max_lbl))) ;
            res () ]
        else
          if_
            ~cond:(eq (u16 (Uint16.of_int i)) cstr)
            ~then_:(res ())
            ~else_:(choose_cstr (i + 1)) in
      choose_cstr 0)

  and dunit _ _ _ src =
    pair unit src

  and make1 dstate mn0 path mn src =
    let rec des_of_vt = function
      | T.Unknown -> invalid_arg "make1"
      | T.Unit -> dunit
      | T.Mac TFloat -> Des.dfloat
      | T.Mac TString -> Des.dstring
      | T.Mac TBool -> Des.dbool
      | T.Mac TChar -> Des.dchar
      | T.Mac TI8 -> Des.di8
      | T.Mac TI16 -> Des.di16
      | T.Mac TI24 -> Des.di24
      | T.Mac TI32 -> Des.di32
      | T.Mac TI40 -> Des.di40
      | T.Mac TI48 -> Des.di48
      | T.Mac TI56 -> Des.di56
      | T.Mac TI64 -> Des.di64
      | T.Mac TI128 -> Des.di128
      | T.Mac TU8 -> Des.du8
      | T.Mac TU16 -> Des.du16
      | T.Mac TU24 -> Des.du24
      | T.Mac TU32 -> Des.du32
      | T.Mac TU40 -> Des.du40
      | T.Mac TU48 -> Des.du48
      | T.Mac TU56 -> Des.du56
      | T.Mac TU64 -> Des.du64
      | T.Mac TU128 -> Des.du128
      | T.Usr vt -> des_of_vt vt.def
      | T.TTup mns -> dtup mns
      | T.TRec mns -> drec mns
      | T.TSum mns -> dsum mns
      | T.TVec (dim, mn) -> dvec dim mn
      | T.TList mn -> dlist mn
      | T.TSet mn -> dset mn
      | T.TMap _ -> assert false (* No value of map type *)
    in
    let vt = mn.vtyp in
    if mn.nullable then (
      let cond = Des.is_null dstate mn0 path src in
      if_ ~cond
        ~then_:(pair (null vt) (Des.dnull vt dstate mn0 path src))
        ~else_:(
          let src = Des.dnotnull vt dstate mn0 path src in
          let des = des_of_vt vt in
          let v_src = des dstate mn0 path src in
          E.with_sploded_pair "make1_1" v_src (fun v src ->
            pair (to_nullable v) src))
    ) else (
      let des = des_of_vt vt in
      des dstate mn0 path src
    )

  let rec make ?config mn0 src =
    let dstate, src = Des.start ?config mn0 src in
    E.with_sploded_pair "make" (make1 dstate mn0 [] mn0 src) (fun v src ->
      pair v (Des.stop dstate src))
end

(* The other way around: given a heap value of some type and a serializer,
 * serialize that value: *)

module Serialize (Ser :SER) : sig
    val serialize : ?config:Ser.config -> T.maybe_nullable -> E.t (*ma*) -> E.t (*v*) -> (*dst*) E.t -> (*dst*) E.t
    val sersize : T.maybe_nullable -> E.t (*ma*) -> E.t (*v*) -> (*size*size*) E.t
  end =
struct

  let rec svec dim mn sstate mn0 path v dst =
    let dst = Ser.vec_opn sstate mn0 path dim mn dst in
    let rec loop i dst =
      let subpath = T.path_append i path in
      if i >= dim then
        Ser.vec_cls sstate mn0 path dst
      else
        let dst = if i = 0 then dst else
                    Ser.vec_sep sstate mn0 subpath dst in
        let_ "dst" dst ~in_:(
          let v' = nth (u32_of_int i) v in
          ser1 sstate mn0 subpath mn v' copy_field (identifier "dst") |>
          loop (i + 1)) in
    loop 0 dst

  and slist_or_set mn sstate mn0 path v dst =
    let len = cardinality v in
    let dst = Ser.list_opn sstate mn0 path mn (Some len) dst in
    let dst_n =
      let subpath = T.path_append 0 path in
      fold ~lst:v
        ~init:(pair dst (i32 0l))
        ~body:
          (E.func2 T.(TPair (Ser.ptr mn0, T.i32)) (T.TValue mn)
                   (fun _l dst_n x ->
            E.with_sploded_pair "dst_n" dst_n (fun dst n ->
              let_ "dst" (
                if_ ~cond:(gt n (i32 0l))
                    ~then_:(Ser.list_sep sstate mn0 subpath dst)
                    ~else_:dst)
                ~in_:(
                  pair
                    (ser1 sstate mn0 subpath mn x copy_field
                          (identifier "dst"))
                    (add (i32 1l) n))))) in
    Ser.list_cls sstate mn0 path (first dst_n)

  and stup mns ma sstate mn0 path v dst =
    let dst = Ser.tup_opn sstate mn0 path mns dst in
    (* this returns a new mask that is going to be read entirely *)
    let m = mask_enter (Array.length mns) ma in
    let dst =
      Array.fold_lefti (fun dst i mn ->
        let subpath = T.path_append i path in
        let_ "dst"
          (if i = 0 then dst else
                    Ser.tup_sep sstate mn0 subpath dst)
          ~in_:(ser1 sstate mn0 subpath mn (get_item i v) (mask_get i m)
                     (identifier "dst"))
      ) dst mns in
    Ser.tup_cls sstate mn0 path dst

  and srec mns ma sstate mn0 path v dst =
    let dst = Ser.rec_opn sstate mn0 path mns dst in
    let m = mask_enter (Array.length mns) ma in
    let dst =
      Array.fold_lefti (fun dst i (field, mn) ->
        let subpath = T.path_append i path in
        let_ "dst"
          (if i = 0 then dst else
                    Ser.rec_sep sstate mn0 subpath dst)
          ~in_:(comment ("serialize field "^ field)
                  (ser1 sstate mn0 subpath mn (get_field field v) (mask_get i m)
                        (identifier "dst")))
      ) dst mns in
    Ser.rec_cls sstate mn0 path dst

  (* Regarding masks, sum types are like scalars: all or nothing.
   * Indeed, the mask would have to be aware of the constructor to be
   * meaningful. *)
  and ssum mns ma sstate mn0 path v dst =
    let max_lbl = Array.length mns - 1 in
    let dst =
      let_ "label"
        (label_of v)
        ~in_:(
          let_ "dst"
            (Ser.sum_opn sstate mn0 path mns (identifier "label") dst)
            ~in_:(
              let rec choose_cstr i =
                let subpath = T.path_append i path in
                assert (i <= max_lbl) ;
                let field, mn = mns.(i) in
                if i = max_lbl then
                  seq [
                    assert_ (eq (identifier "label") (u16 (Uint16.of_int max_lbl))) ;
                    ser1 sstate mn0 subpath mn (get_alt field v) ma
                         (identifier "dst") ]
                else
                  if_
                    ~cond:(eq (u16 (Uint16.of_int i)) (identifier "label"))
                    ~then_:(ser1 sstate mn0 subpath mn (get_alt field v) ma
                                 (identifier "dst"))
                    ~else_:(choose_cstr (i + 1)) in
              choose_cstr 0)) in
    Ser.sum_cls sstate mn0 path dst

  and sunit _ _ _ _ dst = dst

  and ser1 sstate mn0 path mn v ma dst =
    let rec ser_of_vt = function
      | T.Unknown -> invalid_arg "ser1"
      | T.Unit -> sunit
      | T.Mac TFloat -> Ser.sfloat
      | T.Mac TString -> Ser.sstring
      | T.Mac TBool -> Ser.sbool
      | T.Mac TChar -> Ser.schar
      | T.Mac TI8 -> Ser.si8
      | T.Mac TI16 -> Ser.si16
      | T.Mac TI24 -> Ser.si24
      | T.Mac TI32 -> Ser.si32
      | T.Mac TI40 -> Ser.si40
      | T.Mac TI48 -> Ser.si48
      | T.Mac TI56 -> Ser.si56
      | T.Mac TI64 -> Ser.si64
      | T.Mac TI128 -> Ser.si128
      | T.Mac TU8 -> Ser.su8
      | T.Mac TU16 -> Ser.su16
      | T.Mac TU24 -> Ser.su24
      | T.Mac TU32 -> Ser.su32
      | T.Mac TU40 -> Ser.su40
      | T.Mac TU48 -> Ser.su48
      | T.Mac TU56 -> Ser.su56
      | T.Mac TU64 -> Ser.su64
      | T.Mac TU128 -> Ser.su128
      | T.Usr vt -> ser_of_vt vt.def
      | T.TTup mns -> stup mns ma
      | T.TRec mns -> srec mns ma
      | T.TSum mns -> ssum mns ma
      | T.TVec (dim, mn) -> svec dim mn
      (* Thanks to cardinality and fold being generic, lists and sets are
       * serialized the same: *)
      | T.TList mn -> slist_or_set mn
      | T.TSet mn -> slist_or_set mn
      | T.TMap _ -> assert false (* No value of map type *)
    in
    if_ ~cond:(eq ma skip_field)
      ~then_:dst
      ~else_:(
        if_ ~cond:(eq ma set_field_null)
          ~then_:(
            if mn.nullable then
              Ser.snull mn.vtyp sstate mn0 path dst
            else
              seq [ assert_ (bool false) ; (* Mask has been type checked *)
                    dst ])
          ~else_:(
            (* Copy or Recurse are handled the same: *)
            let vt = mn.vtyp in
            if mn.nullable then
              let cond = is_null v in
              if_ ~cond
                ~then_:(Ser.snull vt sstate mn0 path dst)
                ~else_:(
                  let dst = Ser.snotnull vt sstate mn0 path dst in
                  let ser = ser_of_vt vt in
                  ser sstate mn0 path (to_not_nullable v) dst)
            else
              let ser = ser_of_vt vt in
              ser sstate mn0 path v dst))

  and serialize ?config mn0 ma v dst =
    let path = [] in
    let sstate, dst = Ser.start ?config mn0 dst in
    let dst = ser1 sstate mn0 path mn0 v ma dst in
    Ser.stop sstate dst

  (*
   * Compute the sersize of a expression:
   *
   * Returns a pair of size identifier holding the const and dyn size of
   * the heap value pointed by the pointer identifier [src].
   * [src] must be a pointer to a heap value, as returned by the above
   * Ser module.
   *)

  let sizes_t = T.TPair (TSize, TSize)

  let add_size sizes sz =
(*    map_pair sizes
      (E.func2 T.size T.size (fun _l s1 s2 ->
        match sz with
        | ConstSize s ->
            pair (add (size s) s1) s2
        | DynSize s ->
            pair s1 (add s s2))) *)
    E.with_sploded_pair "add_size" sizes (fun cstsz dynsz ->
      match sz with
      | ConstSize s ->
          pair (add (size s) cstsz) dynsz
      | DynSize s ->
          pair cstsz (add s dynsz))

  let rec ssvec dim mn mn0 path v sizes =
    let sizes =
      Ser.ssize_of_vec mn0 path v |> add_size sizes in
    let rec loop sizes i =
      if i >= dim then sizes else
      let subpath = T.path_append i path in
      let v' = nth (u32_of_int i) v in
      let_ "sizes" sizes ~in_:(
        let sizes = sersz1 mn mn0 subpath v' copy_field (identifier "sizes") in
        loop sizes (i + 1)) in
    loop sizes 0

  and sslist mn mn0 path v sizes =
    let sizes =
      Ser.ssize_of_list mn0 path v |> add_size sizes in
    let len = cardinality v in
    (* TODO: a way to ask only for the dynsize, and compute the constsize
     * as len * const_size of mn *)
    let subpath = T.path_append 0 path in (* good enough *)
    repeat ~from:(i32 0l) ~to_:(to_i32 len)
      ~body:
        (E.func2 T.i32 sizes_t (fun _l n sizes ->
          let v' = nth n v in
          sersz1 mn mn0 subpath v' copy_field sizes))
      ~init:sizes

  and sstup mns ma mn0 path v sizes =
    let sizes =
      Ser.ssize_of_tup mn0 path v |> add_size sizes in
    let m = mask_enter (Array.length mns) ma in
    Array.fold_lefti (fun sizes i mn ->
      let v' = get_item i v in
      let ma = mask_get i m in
      let subpath = T.path_append i path in
      let_ "sizes" sizes
        ~in_:(sersz1 mn mn0 subpath v' ma (identifier "sizes"))
    ) sizes mns

  and ssrec mns ma mn0 path v sizes =
    let sizes =
      Ser.ssize_of_rec mn0 path v |> add_size sizes in
    let m = mask_enter (Array.length mns) ma in
    Array.fold_lefti (fun sizes i (n, mn) ->
      let v' = get_field n v in
      let ma = mask_get i m in
      let subpath = T.path_append i path in
      let_ "sizes" sizes
        ~in_:(sersz1 mn mn0 subpath v' ma (identifier "sizes"))
    ) sizes mns

  and sssum mns mn0 path v sizes =
    let sizes =
      Ser.ssize_of_sum mn0 path v |> add_size sizes in
    let max_lbl = Array.length mns - 1 in
    let_ "label"
      (label_of v)
      ~in_:(
        let rec choose_cstr i =
          let name, mn = mns.(i) in
          let v' = get_alt name v in
          let subpath = T.path_append i path in
          assert (i <= max_lbl) ;
          if i = max_lbl then
            seq [
              assert_ (eq (identifier "label") (u16 (Uint16.of_int max_lbl))) ;
              sersz1 mn mn0 subpath v' copy_field sizes ]
          else
            if_
              ~cond:(eq (u16 (Uint16.of_int i)) (identifier "label"))
              ~then_:(sersz1 mn mn0 subpath v' copy_field sizes)
              ~else_:(choose_cstr (i + 1)) in
        choose_cstr 0)

  and sersz1 mn mn0 path v ma sizes =
    let to_dyn ssizer mn0 path v sizes =
      let sz = ssizer mn0 path v in
      add_size sizes sz in
    let rec ssz_of_vt = function
      | T.Unknown -> invalid_arg "sersz1"
      | T.Unit -> fun _ _ _ sizes -> sizes
      | T.Mac TFloat -> to_dyn Ser.ssize_of_float
      | T.Mac TString -> to_dyn Ser.ssize_of_string
      | T.Mac TBool -> to_dyn Ser.ssize_of_bool
      | T.Mac TChar -> to_dyn Ser.ssize_of_char
      | T.Mac TI8 -> to_dyn Ser.ssize_of_i8
      | T.Mac TI16 -> to_dyn Ser.ssize_of_i16
      | T.Mac TI24 -> to_dyn Ser.ssize_of_i24
      | T.Mac TI32 -> to_dyn Ser.ssize_of_i32
      | T.Mac TI40 -> to_dyn Ser.ssize_of_i40
      | T.Mac TI48 -> to_dyn Ser.ssize_of_i48
      | T.Mac TI56 -> to_dyn Ser.ssize_of_i56
      | T.Mac TI64 -> to_dyn Ser.ssize_of_i64
      | T.Mac TI128 -> to_dyn Ser.ssize_of_i128
      | T.Mac TU8 -> to_dyn Ser.ssize_of_u8
      | T.Mac TU16 -> to_dyn Ser.ssize_of_u16
      | T.Mac TU24 -> to_dyn Ser.ssize_of_u24
      | T.Mac TU32 -> to_dyn Ser.ssize_of_u32
      | T.Mac TU40 -> to_dyn Ser.ssize_of_u40
      | T.Mac TU48 -> to_dyn Ser.ssize_of_u48
      | T.Mac TU56 -> to_dyn Ser.ssize_of_u56
      | T.Mac TU64 -> to_dyn Ser.ssize_of_u64
      | T.Mac TU128 -> to_dyn Ser.ssize_of_u128
      | T.Usr vt -> ssz_of_vt vt.def
      | T.TVec (dim, mn) -> ssvec dim mn
      | T.TTup mns -> sstup mns ma
      | T.TRec mns -> ssrec mns ma
      | T.TSum mns -> sssum mns
      | T.TList mn -> sslist mn
      (* Sets are serialized like lists: *)
      | T.TSet mn -> sslist mn
      | T.TMap _ -> assert false (* No value of map type *)
    in
    if_ ~cond:(eq ma skip_field)
      ~then_:sizes
      ~else_:(
        if_ ~cond:(eq ma set_field_null)
          ~then_:(
            if mn.nullable then
              add_size sizes (Ser.ssize_of_null mn0 path)
            else
              seq [ assert_ (bool false) ;
                    sizes ])
          ~else_:(
            let vt = mn.vtyp in
            if mn.nullable then
              if_ ~cond:(is_null v)
                ~then_:(add_size sizes (Ser.ssize_of_null mn0 path))
                ~else_:(ssz_of_vt vt mn0 path (to_not_nullable v) sizes)
            else
              ssz_of_vt vt mn0 path v sizes))

  let sersize mn ma v =
    let sizes = pair (size 0) (size 0) in
    sersz1 mn mn [] v ma sizes

end
