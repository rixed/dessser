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

module type MATERIALIZE =
sig
  module Des : DES

  val make : ?config:Des.config ->
             T.maybe_nullable ->
             E.env ->
             (*src*) E.t ->
             (*e*src*) E.t
end

module Materialize (Des : DES) :
  MATERIALIZE with module Des = Des =
struct
  module Des = Des

  let rec dvec dim mn dstate mn0 path l src =
    let src = Des.vec_opn dstate mn0 path dim mn l src in
    let rec loop ids i l src =
      if i >= dim then
        pair
          (make_vec (List.rev ids))
          (Des.vec_cls dstate mn0 path l src)
      else
        let src = if i = 0 then src else
                    Des.vec_sep dstate mn0 path l src in
        let subpath = T.path_append i path in
        let v_src = make1 dstate mn0 subpath mn l src in
        E.with_sploded_pair ~l "dvec" v_src (fun l v src ->
          loop (v :: ids) (i + 1) l src) in
    loop [] 0 l src

  (* Lists and sets are serialized the same, so here the list is desserialized
   * into an slist, and then the final operation converts it into a list or a
   * set: *)

  and dlist mn dstate mn0 path l src =
    dslist list_of_slist_rev mn dstate mn0 path l src

  and dset mn dstate mn0 path l src =
    dslist set_of_slist mn dstate mn0 path l src

  and dslist of_slist mn dstate mn0 path l src =
    let init_t = T.Data mn in
    let init_list_t = T.SList init_t in
    let inits_src_t = T.Pair (init_list_t, Des.ptr mn0) in
    (* good enough to determine the item type but not much more: *)
    let subpath = T.path_append 0 path in
    match Des.list_opn dstate with
    | KnownSize list_opn ->
        let dim_src = list_opn mn0 path mn l src in
        let inits_src =
          E.with_sploded_pair ~l "dlist1" dim_src (fun l dim src ->
            repeat ~from:(i32 0l) ~to_:(to_i32 dim)
              ~body:
                (E.func2 ~l T.i32 inits_src_t (fun l n inits_src ->
                  E.with_sploded_pair ~l "dlist2" inits_src (fun l inits src ->
                    let src =
                      if_ (eq n (i32 0l))
                        ~then_:src
                        ~else_:(Des.list_sep dstate mn0 path l src) in
                    let v_src = make1 dstate mn0 subpath mn l src in
                    E.with_sploded_pair ~l "dlist3" v_src (fun _l v src ->
                      pair (cons v inits) src))))
              ~init:(pair (end_of_list init_t) src)) in
        E.with_sploded_pair ~l "dlist4" inits_src (fun l inits src ->
          let v = of_slist inits
          and src = Des.list_cls dstate mn0 path l src in
          pair v src)
    | UnknownSize (list_opn, is_end_of_list) ->
        let fst_inits_src_t = T.Pair (T.bool, inits_src_t) in
        let src = list_opn mn0 path mn l src in
        let fst_inits_src =
          loop_while
            ~cond:
              (E.func1 ~l fst_inits_src_t (fun l fst_inits_src ->
                let src = secnd (secnd fst_inits_src) in
                not_ (is_end_of_list mn0 path l src)))
            ~body:
              (E.func1 ~l fst_inits_src_t (fun l fst_inits_src ->
                E.with_sploded_pair ~l "dlist5" fst_inits_src (fun l is_fst inits_src ->
                  E.with_sploded_pair ~l "dlist6" inits_src (fun l inits src ->
                    let src =
                      if_ is_fst
                        ~then_:src
                        ~else_:(Des.list_sep dstate mn0 path l src) in
                    let v_src = make1 dstate mn0 subpath mn l src in
                    let inits_src =
                      E.with_sploded_pair ~l "dlist7" v_src (fun _l v src ->
                        pair (cons v inits) src) in
                    pair false_ inits_src))))
            ~init:(pair true_ (pair (end_of_list init_t) src)) in
        E.with_sploded_pair ~l "dlist9" (secnd fst_inits_src) (fun l inits src ->
          let v = of_slist inits
          and src = Des.list_cls dstate mn0 path l src in
          pair v src)

  and dtup mns dstate mn0 path l src =
    let src = Des.tup_opn dstate mn0 path mns l src in
    let rec loop ids i l src =
      if i >= Array.length mns then
        pair
          (make_tup (List.rev ids))
          (Des.tup_cls dstate mn0 path l src)
      else
        let src = if i = 0 then src else
                    Des.tup_sep dstate mn0 path l src in
        let subpath = T.path_append i path in
        let v_src = make1 dstate mn0 subpath mns.(i) l src in
        E.with_sploded_pair ~l "dtup" v_src (fun l v src ->
          loop (v :: ids) (i + 1) l src) in
    loop [] 0 l src

  and drec mns dstate mn0 path l src =
    let src = Des.rec_opn dstate mn0 path mns l src in
    let len = Array.length mns in
    let rec loop ids i l src =
      if i >= len then
        pair
          (make_rec (List.mapi (fun i id ->
             fst mns.(len - i - 1), id) ids))
          (Des.rec_cls dstate mn0 path l src)
      else
        let src = if i = 0 then src else
                    Des.rec_sep dstate mn0 path l src in
        let subpath = T.path_append i path in
        let v_src = make1 dstate mn0 subpath (snd mns.(i)) l src in
        E.with_sploded_pair ~l "drec" v_src (fun l v src ->
          loop (v :: ids) (i + 1) l src) in
    loop [] 0 l src

  and dsum mns dstate mn0 path l src =
    let cstr_src = Des.sum_opn dstate mn0 path mns l src in
    let max_lbl = Array.length mns - 1 in
    E.with_sploded_pair ~l "dsum1" cstr_src (fun l cstr src ->
      let rec choose_cstr i =
        assert (i <= max_lbl) ;
        let res () =
          let subpath = T.path_append i path in
          let _, subtyp = mns.(i) in
          let v_src = make1 dstate mn0 subpath subtyp l src in
          E.with_sploded_pair ~l "dsum2" v_src (fun l v src ->
            pair
              (construct mns i v)
              (Des.sum_cls dstate mn0 path l src))
        in
        if i = max_lbl then
          seq [
            assert_ (eq cstr (u16 (Uint16.of_int max_lbl))) ;
            res () ]
        else
          if_ (eq (u16 (Uint16.of_int i)) cstr)
            ~then_:(res ())
            ~else_:(choose_cstr (i + 1)) in
      choose_cstr 0)

  and dunit _ _ _ _ src =
    pair unit src

  and make1 dstate mn0 path mn l src =
    let rec des_of_vt = function
      | T.Unknown | T.Ext _ -> invalid_arg "make1"
      | T.Base Unit -> dunit
      | T.Base Float -> Des.dfloat
      | T.Base String -> Des.dstring
      | T.Base Bool -> Des.dbool
      | T.Base Char -> Des.dchar
      | T.Base I8 -> Des.di8
      | T.Base I16 -> Des.di16
      | T.Base I24 -> Des.di24
      | T.Base I32 -> Des.di32
      | T.Base I40 -> Des.di40
      | T.Base I48 -> Des.di48
      | T.Base I56 -> Des.di56
      | T.Base I64 -> Des.di64
      | T.Base I128 -> Des.di128
      | T.Base U8 -> Des.du8
      | T.Base U16 -> Des.du16
      | T.Base U24 -> Des.du24
      | T.Base U32 -> Des.du32
      | T.Base U40 -> Des.du40
      | T.Base U48 -> Des.du48
      | T.Base U56 -> Des.du56
      | T.Base U64 -> Des.du64
      | T.Base U128 -> Des.du128
      | T.Usr vt ->
          (* Deserialize according to vt.def, then make a new user value to
           * keep the user type: *)
          fun state mn path l ptr ->
            let v_src = des_of_vt vt.def state mn path l ptr in
            E.with_sploded_pair ~l "des_usr_type" v_src (fun _l v src ->
              pair (make_usr vt.name [ v ]) src)
      | T.Tup mns -> dtup mns
      | T.Rec mns -> drec mns
      | T.Sum mns -> dsum mns
      | T.Vec (dim, mn) -> dvec dim mn
      | T.Lst mn -> dlist mn
      | T.Set (Simple, mn) -> dset mn
      | T.Set _ -> todo "Materialization of non simple sets"
      | T.Map _ -> assert false (* No value of map type *)
    in
    let vt = mn.vtyp in
    if mn.nullable then (
      if_ (Des.is_null dstate mn0 path l src)
        ~then_:(pair (null vt) (Des.dnull vt dstate mn0 path l src))
        ~else_:(
          let src = Des.dnotnull vt dstate mn0 path l src in
          let des = des_of_vt vt in
          let v_src = des dstate mn0 path l src in
          E.with_sploded_pair ~l "make1_1" v_src (fun _l v src ->
            pair (not_null v) src))
    ) else (
      let des = des_of_vt vt in
      des dstate mn0 path l src
    )

  let make ?config mn0 l src =
    let_ ~name:"src" ~l src (fun l src ->
      let dstate, src = Des.start ?config mn0 l src in
      E.with_sploded_pair ~l "make" (make1 dstate mn0 [] mn0 l src) (fun l v src ->
        pair v (Des.stop dstate l src)))
end

(* The other way around: given a heap value of some type and a serializer,
 * serialize that value: *)

module type SERIALIZE =
sig
  module Ser : SER

  val serialize : ?config:Ser.config ->
                  T.maybe_nullable ->
                  E.env ->
                  E.t (*ma*) ->
                  E.t (*v*) ->
                  (*dst*) E.t ->
                  (*dst*) E.t

  val sersize : ?config:Ser.config ->
                T.maybe_nullable ->
                E.env ->
                E.t (*ma*) ->
                E.t (*v*) ->
                (*size*size*) E.t
end

module Serialize (Ser : SER) :
  SERIALIZE with module Ser = Ser =
struct
  module Ser = Ser

  let rec svec dim mn sstate mn0 path l v dst =
    let dst = Ser.vec_opn sstate mn0 path dim mn l dst in
    let rec loop i l dst =
      let subpath = T.path_append i path in
      if i >= dim then
        Ser.vec_cls sstate mn0 path l dst
      else
        let dst = if i = 0 then dst else
                    Ser.vec_sep sstate mn0 subpath l dst in
        let_ ~name:"svec_dst" ~l dst (fun l dst ->
          let v' = nth (u32_of_int i) v in
          ser1 sstate mn0 subpath mn l v' copy_field dst |>
          loop (i + 1) l) in
    loop 0 l dst

  and slist_or_set mn sstate mn0 path l v dst =
    let len = cardinality v in
    let dst = Ser.list_opn sstate mn0 path mn (Some len) l dst in
    let dst_n =
      let subpath = T.path_append 0 path in
      fold ~list:v
        ~init:(pair dst (i32 0l))
        ~body:
          (E.func2 ~l T.(Pair (Ser.ptr mn0, T.i32)) (T.Data mn)
                   (fun l dst_n x ->
            E.with_sploded_pair ~l "dst_n" dst_n (fun l dst n ->
              let_ ~name:"slist_dst" ~l (
                if_ (gt n (i32 0l))
                    ~then_:(Ser.list_sep sstate mn0 subpath l dst)
                    ~else_:dst)
                (fun l dst ->
                  pair
                    (ser1 sstate mn0 subpath mn l x copy_field dst)
                    (add (i32 1l) n))))) in
    Ser.list_cls sstate mn0 path l (first dst_n)

  and stup mns ma sstate mn0 path l v dst =
    let dst = Ser.tup_opn sstate mn0 path mns l dst in
    let dst =
      Array.fold_lefti (fun dst i mn ->
        let subpath = T.path_append i path in
        let_ ~name:"stup_dst" ~l
          (if i = 0 then dst else
                    Ser.tup_sep sstate mn0 subpath l dst)
          (fun l dst ->
            ser1 sstate mn0 subpath mn l (get_item i v) (mask_get i ma) dst)
      ) dst mns in
    Ser.tup_cls sstate mn0 path l dst

  and srec mns ma sstate mn0 path l v dst =
    let dst = Ser.rec_opn sstate mn0 path mns l dst in
    let dst =
      Array.fold_lefti (fun dst i (fname, mn) ->
        let subpath = T.path_append i path in
        let_ ~name:"srec_dst" ~l
          (if i = 0 then dst else
                    Ser.rec_sep sstate mn0 subpath l dst)
          (fun l dst ->
            comment ("serialize field "^ fname)
                    (ser1 sstate mn0 subpath mn l (get_field fname v)
                          (mask_get i ma) dst))
      ) dst mns in
    Ser.rec_cls sstate mn0 path l dst

  (* Regarding masks, sum types are like scalars: all or nothing.
   * Indeed, the mask would have to be aware of the constructor to be
   * meaningful. *)
  and ssum mns ma sstate mn0 path l v dst =
    let max_lbl = Array.length mns - 1 in
    let dst =
      let_ ~name:"label1" ~l
        (label_of v)
        (fun l label ->
          let_ ~name:"ssum_dst" ~l
            (Ser.sum_opn sstate mn0 path mns l label dst)
            (fun l dst ->
              let rec choose_cstr i =
                let subpath = T.path_append i path in
                assert (i <= max_lbl) ;
                let field, mn = mns.(i) in
                let v' = get_alt field v in
                if i = max_lbl then
                  seq [
                    assert_ (eq label (u16 (Uint16.of_int max_lbl))) ;
                    ser1 sstate mn0 subpath mn l v' ma dst ]
                else
                  if_ (eq (u16 (Uint16.of_int i)) label)
                    ~then_:(ser1 sstate mn0 subpath mn l v' ma dst)
                    ~else_:(choose_cstr (i + 1)) in
              choose_cstr 0)) in
    Ser.sum_cls sstate mn0 path l dst

  and sunit _ _ _ _ _ dst = dst

  and ser1 sstate mn0 path mn l v ma dst =
    let rec ser_of_vt = function
      | T.Unknown | T.Ext _ -> invalid_arg "ser1"
      | T.Base Unit -> sunit
      | T.Base Float -> Ser.sfloat
      | T.Base String -> Ser.sstring
      | T.Base Bool -> Ser.sbool
      | T.Base Char -> Ser.schar
      | T.Base I8 -> Ser.si8
      | T.Base I16 -> Ser.si16
      | T.Base I24 -> Ser.si24
      | T.Base I32 -> Ser.si32
      | T.Base I40 -> Ser.si40
      | T.Base I48 -> Ser.si48
      | T.Base I56 -> Ser.si56
      | T.Base I64 -> Ser.si64
      | T.Base I128 -> Ser.si128
      | T.Base U8 -> Ser.su8
      | T.Base U16 -> Ser.su16
      | T.Base U24 -> Ser.su24
      | T.Base U32 -> Ser.su32
      | T.Base U40 -> Ser.su40
      | T.Base U48 -> Ser.su48
      | T.Base U56 -> Ser.su56
      | T.Base U64 -> Ser.su64
      | T.Base U128 -> Ser.su128
      | T.Usr vt -> ser_of_vt vt.def
      | T.Tup mns -> stup mns ma
      | T.Rec mns -> srec mns ma
      | T.Sum mns -> ssum mns ma
      | T.Vec (dim, mn) -> svec dim mn
      (* Thanks to cardinality and fold being generic, lists and sets are
       * serialized the same: *)
      | T.Lst mn -> slist_or_set mn
      | T.Set (_, mn) -> slist_or_set mn
      | T.Map _ -> assert false (* No value of map type *)
    in
    if_ (eq ma skip_field)
      ~then_:dst
      ~else_:(
        if_ (eq ma set_field_null)
          ~then_:(
            if mn.nullable then
              Ser.snull mn.vtyp sstate mn0 path l dst
            else
              seq [ assert_ false_ ; (* Mask has been type checked *)
                    dst ])
          ~else_:(
            (* Copy or Recurse are handled the same: *)
            let vt = mn.vtyp in
            if mn.nullable then
              if_null v
                ~then_:(Ser.snull vt sstate mn0 path l dst)
                ~else_:(
                  let dst = Ser.snotnull vt sstate mn0 path l dst in
                  let ser = ser_of_vt vt in
                  ser sstate mn0 path l (force v) dst)
            else
              let ser = ser_of_vt vt in
              ser sstate mn0 path l v dst))

  and serialize ?config mn0 l ma v dst =
    let path = [] in
    let_ ~name:"ma" ~l ma (fun l ma ->
      let_ ~name:"v" ~l v (fun l v ->
        let_ ~name:"ser_dst" ~l dst (fun l dst ->
          let sstate, dst = Ser.start ?config mn0 l dst in
          let dst = ser1 sstate mn0 path mn0 l v ma dst in
          Ser.stop sstate l dst)))

  (*
   * Compute the sersize of a expression:
   *
   * Returns a pair of size identifier holding the const and dyn size of
   * the heap value pointed by the pointer identifier [src].
   * [src] must be a pointer to a heap value, as returned by the above
   * Ser module.
   *)

  let sizes_t = T.Pair (Size, Size)

  let add_size l sizes sz =
(*    map_pair sizes
      (E.func2 T.size T.size (fun _l s1 s2 ->
        match sz with
        | ConstSize s ->
            pair (add (size s) s1) s2
        | DynSize s ->
            pair s1 (add s s2))) *)
    E.with_sploded_pair ~l "add_size" sizes (fun _l cstsz dynsz ->
      match sz with
      | ConstSize s ->
          pair (add (size s) cstsz) dynsz
      | DynSize s ->
          pair cstsz (add s dynsz))

  let rec ssvec dim mn mn0 path l v sizes =
    let sizes =
      Ser.ssize_of_vec mn0 path l v |> add_size l sizes in
    let rec loop l sizes i =
      if i >= dim then sizes else
      let subpath = T.path_append i path in
      let v' = nth (u32_of_int i) v in
      let_ ~name:"sizes" ~l sizes (fun l sizes ->
        let sizes = sersz1 mn mn0 subpath l v' copy_field sizes in
        loop l sizes (i + 1)) in
    loop l sizes 0

  and sslist mn mn0 path l v sizes =
    let sizes =
      Ser.ssize_of_list mn0 path l v |> add_size l sizes in
    let len = cardinality v in
    (* TODO: a way to ask only for the dynsize, and compute the constsize
     * as len * const_size of mn *)
    let subpath = T.path_append 0 path in (* good enough *)
    repeat ~from:(i32 0l) ~to_:(to_i32 len)
      ~body:
        (E.func2 ~l T.i32 sizes_t (fun l n sizes ->
          let v' = nth n v in
          sersz1 mn mn0 subpath l v' copy_field sizes))
      ~init:sizes

  and sstup mns ma mn0 path l v sizes =
    let sizes =
      Ser.ssize_of_tup mn0 path l v |> add_size l sizes in
    Array.fold_lefti (fun sizes i mn ->
      let v' = get_item i v in
      let ma = mask_get i ma in
      let subpath = T.path_append i path in
      let_ ~name:"sizes" ~l sizes (fun l sizes ->
        sersz1 mn mn0 subpath l v' ma sizes)
    ) sizes mns

  and ssrec mns ma mn0 path l v sizes =
    let sizes =
      Ser.ssize_of_rec mn0 path l v |> add_size l sizes in
    Array.fold_lefti (fun sizes i (fname, mn) ->
      let v' = get_field fname v in
      let ma = mask_get i ma in
      let subpath = T.path_append i path in
      let_ ~name:"sizes" ~l sizes (fun l sizes ->
        comment ("sersize of field "^ fname)
                (sersz1 mn mn0 subpath l v' ma sizes))
    ) sizes mns

  and sssum mns mn0 path l v sizes =
    let sizes =
      Ser.ssize_of_sum mn0 path l v |> add_size l sizes in
    let max_lbl = Array.length mns - 1 in
    let_ ~name:"label2" ~l
      (label_of v)
      (fun l label ->
        let rec choose_cstr i =
          let name, mn = mns.(i) in
          let v' = get_alt name v in
          let subpath = T.path_append i path in
          assert (i <= max_lbl) ;
          if i = max_lbl then
            seq [
              assert_ (eq label (u16 (Uint16.of_int max_lbl))) ;
              sersz1 mn mn0 subpath l v' copy_field sizes ]
          else
            if_ (eq (u16 (Uint16.of_int i)) label)
              ~then_:(sersz1 mn mn0 subpath l v' copy_field sizes)
              ~else_:(choose_cstr (i + 1)) in
        choose_cstr 0)

  and sersz1 mn mn0 path l v ma sizes =
    let to_dyn ssizer mn0 path l v sizes =
      let sz = ssizer mn0 path l v in
      add_size l sizes sz in
    let rec ssz_of_vt = function
      | T.Unknown | T.Ext _ -> invalid_arg "sersz1"
      | T.Base Unit -> fun _ _ _ _ sizes -> sizes
      | T.Base Float -> to_dyn Ser.ssize_of_float
      | T.Base String -> to_dyn Ser.ssize_of_string
      | T.Base Bool -> to_dyn Ser.ssize_of_bool
      | T.Base Char -> to_dyn Ser.ssize_of_char
      | T.Base I8 -> to_dyn Ser.ssize_of_i8
      | T.Base I16 -> to_dyn Ser.ssize_of_i16
      | T.Base I24 -> to_dyn Ser.ssize_of_i24
      | T.Base I32 -> to_dyn Ser.ssize_of_i32
      | T.Base I40 -> to_dyn Ser.ssize_of_i40
      | T.Base I48 -> to_dyn Ser.ssize_of_i48
      | T.Base I56 -> to_dyn Ser.ssize_of_i56
      | T.Base I64 -> to_dyn Ser.ssize_of_i64
      | T.Base I128 -> to_dyn Ser.ssize_of_i128
      | T.Base U8 -> to_dyn Ser.ssize_of_u8
      | T.Base U16 -> to_dyn Ser.ssize_of_u16
      | T.Base U24 -> to_dyn Ser.ssize_of_u24
      | T.Base U32 -> to_dyn Ser.ssize_of_u32
      | T.Base U40 -> to_dyn Ser.ssize_of_u40
      | T.Base U48 -> to_dyn Ser.ssize_of_u48
      | T.Base U56 -> to_dyn Ser.ssize_of_u56
      | T.Base U64 -> to_dyn Ser.ssize_of_u64
      | T.Base U128 -> to_dyn Ser.ssize_of_u128
      | T.Usr vt -> ssz_of_vt vt.def
      | T.Vec (dim, mn) -> ssvec dim mn
      | T.Tup mns -> sstup mns ma
      | T.Rec mns -> ssrec mns ma
      | T.Sum mns -> sssum mns
      | T.Lst mn -> sslist mn
      (* Sets are serialized like lists: *)
      | T.Set (_, mn) -> sslist mn
      | T.Map _ -> assert false (* No value of map type *)
    in
    if_ (eq ma skip_field)
      ~then_:sizes
      ~else_:(
        if_ (eq ma set_field_null)
          ~then_:(
            if mn.nullable then
              add_size l sizes (Ser.ssize_of_null mn0 path)
            else
              seq [ assert_ false_ ;
                    sizes ])
          ~else_:(
            let vt = mn.vtyp in
            if mn.nullable then
              if_null v
                ~then_:(add_size l sizes (Ser.ssize_of_null mn0 path))
                ~else_:(ssz_of_vt vt mn0 path l (force v) sizes)
            else
              ssz_of_vt vt mn0 path l v sizes))

  let sersize ?config mn l ma v =
    let sizes = pair (size 0) (size 0) in
    let sizes = add_size l sizes (Ser.ssize_start ?config mn) in
    let_ ~name:"ma" ~l ma (fun l ma ->
      let_ ~name:"v" ~l v (fun l v ->
        sersz1 mn mn [] l v ma sizes))
end
